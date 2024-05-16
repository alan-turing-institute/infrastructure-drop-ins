{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import CMarkGFM
import Control.Exception (SomeException (..), catch)
import Control.Monad (void, when)
import Data.Char (isDigit)
import Data.Either (rights)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import qualified Data.Time.Calendar as C
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, getEnv)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- Global configuration

data Config = Config
  { -- | The file which we are parsing. The GitHub action (drop_in.yaml) clones
    -- the wiki repository into the wiki/ directory, so that's where we look in.
    diFile :: !FilePath,
    -- | Output HTML file when generating webpage.
    webOutputFile :: !FilePath,
    -- | Website of wiki page to redirect people to when they click the link in
    -- Slack
    wikiUrl :: !Text
  }

config :: Config
config =
  Config
    { diFile = "wiki/Schedule.md",
      webOutputFile = "web/drop-in/index.html",
      wikiUrl = "https://github.com/alan-turing-institute/infrastructure-drop-ins/wiki/Schedule"
    }

-- Markdown predicates

-- | Check if a Markdown node is a table.
isTable :: Node -> Bool
isTable (Node _ (TABLE _) _) = True
isTable _ = False

-- | Check if a Markdown node is a heading.
isHeading :: Node -> Bool
isHeading (Node _ (HEADING _) _) = True
isHeading _ = False

-- | Get the text of a Markdown heading. Returns Nothing if the node is not a heading.
getHeadingText :: Node -> Maybe Text
getHeadingText (Node _ (HEADING _) [n]) = case n of
  Node _ (TEXT t) _ -> Just t
  _ -> Just ""
getHeadingText _ = Nothing

-- | Check if a Markdown node is a heading with the given text
isHeadingWithGivenText :: Text -> Node -> Bool
isHeadingWithGivenText t n =
  case getHeadingText n of
    Just t' -> t == t'
    Nothing -> False

-- Table parsing

data DropInSession = DropInSession
  { month :: !C.MonthOfYear,
    day :: !C.DayOfMonth,
    time :: !Text,
    people :: !Text,
    hybridOrRemote :: !Text
  }
  deriving (Eq, Ord, Show)

getNamedSection :: Text -> Node -> Either Text [Node]
getNamedSection sectionName documentContents =
  case documentContents of
    Node _ DOCUMENT [] -> Left "Document was empty"
    Node _ DOCUMENT ns -> do
      let scheduleSection =
            takeWhile (not . isHeading) -- Take until the next heading (or the end)
              . tail -- Drop the schedule heading itself
              . dropWhile (not . isHeadingWithGivenText sectionName) -- Drop all until the desired heading
              $ ns
      when (null scheduleSection) (Left $ "No section called '" <> sectionName <> "' found")
      pure scheduleSection
    _anyOtherNodeType -> error "Invalid document"

getFirstTableFromSection :: [Node] -> Either Text Node
getFirstTableFromSection sectionNodes =
  case filter isTable sectionNodes of
    [] -> Left "No tables were found in the section"
    n@(Node _ (TABLE _) _) : _ -> pure n

getRowsFromTable :: Node -> Either Text [Node]
getRowsFromTable (Node _ (TABLE _) rows) = pure rows
getRowsFromTable _ = Left "Not a table"

getEntriesFromRow :: Node -> Either Text [Text]
getEntriesFromRow (Node _ TABLE_ROW cells) = do
  let getContentsFromCell :: Node -> Either Text Text
      getContentsFromCell (Node _ TABLE_CELL contents) = do
        -- We use a PARAGRAPH hack here to prevent nodeToCommonmark from
        -- inserting extra line breaks between nodes.
        pure . T.strip . nodeToCommonmark [] Nothing $ Node Nothing PARAGRAPH contents
      getContentsFromCell _ = Left "Not a table cell"
  mapM getContentsFromCell cells
getEntriesFromRow _ = Left "Not a table row"

-- | Remove (some) Markdown formatting from a string
cleanMarkdown :: Text -> Text
cleanMarkdown = T.replace "*" "" . T.replace "_" "" . T.strip

parseEntry :: [Text] -> Either Text DropInSession
parseEntry [tDate, tTime, tWho, tWhere] = do
  -- Hacky date parsing (but this whole script is hacky anyway)
  (day', month') <-
    let dateFailedParse = Left $ "Could not parse date from table cell: '" <> tDate <> "'"
     in case T.words tDate of
          _ : dayPlusLetters : month : _ ->
            let maybeDay = readMaybe @Int (T.unpack $ T.takeWhile isDigit dayPlusLetters)
                maybeMonth = case cleanMarkdown (T.toLower month) of
                  m
                    | "jan" `T.isPrefixOf` m -> Just C.January
                    | "feb" `T.isPrefixOf` m -> Just C.February
                    | "mar" `T.isPrefixOf` m -> Just C.March
                    | "apr" `T.isPrefixOf` m -> Just C.April
                    | "may" `T.isPrefixOf` m -> Just C.May
                    | "jun" `T.isPrefixOf` m -> Just C.June
                    | "jul" `T.isPrefixOf` m -> Just C.July
                    | "aug" `T.isPrefixOf` m -> Just C.August
                    | "sep" `T.isPrefixOf` m -> Just C.September
                    | "oct" `T.isPrefixOf` m -> Just C.October
                    | "nov" `T.isPrefixOf` m -> Just C.November
                    | "dec" `T.isPrefixOf` m -> Just C.December
                    | otherwise -> Nothing
             in case (maybeDay, maybeMonth) of
                  (Just d, Just m) -> pure (d, m)
                  _eitherOneFailed -> dateFailedParse
          _anyOtherPattern -> dateFailedParse
  pure $ DropInSession month' day' tTime tWho tWhere
parseEntry _ = Left "Table row had wrong number of entries"

parseAllEntries :: [[Text]] -> Either Text [DropInSession]
parseAllEntries table = do
  let parsedSessions = map parseEntry table
   in case rights parsedSessions of
        [] -> Left "Could not parse any of the rows in the Markdown table"
        xs -> Right xs

-- | Extract the next drop-in session (after a given date, as specified by `m`
-- and `d`). Note that, because this script discards all information about the
-- year, if you run this at the end of December it will say that no talks are
-- available (because anything in January will be interpreted as being in the
-- past). This can be fixed by passing `1 1` as extra arguments to make the
-- script think that it's January 1st: i.e. run `cabal run dropin -- 1 1`.
getNextDropInFromList :: C.MonthOfYear -> C.DayOfMonth -> [DropInSession] -> Either Text DropInSession
getNextDropInFromList m d talks = do
  case sort $ filter (\t -> month t > m || (month t == m && day t >= d)) talks of
    [] -> Left "No upcoming drop-in sessions"
    (t : _) -> Right t

-- The main section

generateWebpage :: C.MonthOfYear -> C.DayOfMonth -> DropInSession -> IO ()
generateWebpage todaym todayd nextDropIn = do
  let dropInIsToday = month nextDropIn == todaym && day nextDropIn == todayd
      html =
        if dropInIsToday
          then
            T.intercalate
              "\n"
              . map T.pack
              $ [ "<html>",
                  "<head>",
                  printf "<meta property=\"og:title\" content=\"Today's Drop-In Session (%d/%d)\"/>" (day nextDropIn) (month nextDropIn),
                  printf "<meta property=\"og:description\" content=\"*Time:* %s\"/>" (time nextDropIn),
                  "<meta property=\"twitter:label1\" value=\"People\"/>",
                  printf "<meta property=\"twitter:data1\" content=\"%s\"/>" (people nextDropIn),
                  "<meta property=\"twitter:label2\" value=\"Hybrid/Remote?\"/>",
                  printf "<meta property=\"twitter:data2\" content=\"%s\"/>" (hybridOrRemote nextDropIn),
                  printf "<meta http-equiv=\"refresh\" content=\"1;url=%s\"/>" (wikiUrl config),
                  "</head>",
                  "<body>",
                  "Redirecting you to the drop-in session schedule...",
                  "</body>",
                  "</html>"
                ]
          else
            T.intercalate
              "\n"
              . map T.pack
              $ [ "<html>",
                  "<head>",
                  printf "<meta property=\"og:title\" content=\"There is no drop-in session today (%d/%d).\"/>" todayd todaym,
                  "<meta property=\"og:description\" content=\"*Time:* N/A\"/>",
                  printf "<meta http-equiv=\"refresh\" content=\"1;url=%s\"/>" (wikiUrl config),
                  "</head>",
                  "<body>",
                  "Redirecting you to the drop-in session schedule...",
                  "</body>",
                  "</html>"
                ]
  createDirectoryIfMissing True (takeDirectory $ webOutputFile config)
  TIO.writeFile (webOutputFile config) html
  if dropInIsToday
    then TIO.putStrLn (T.pack $ printf "Generated webpage for today's drop-in session (%d/%d) in %s." (day nextDropIn) (month nextDropIn) (webOutputFile config))
    else TIO.putStrLn $ "Generated webpage saying that there is no drop-in session today in " <> T.pack (webOutputFile config) <> "."

parseArgs :: IO (C.MonthOfYear, C.DayOfMonth)
parseArgs = do
  let errorInvalidArgs = do
        hPutStrLn stderr "Error: invalid arguments.\nUsage: dropin [MONTH DAY]\n  MONTH and DAY are integers. You can specify them to pretend that today's date is DAY/MONTH."
        exitFailure
  args <- getArgs
  case args of
    -- No arguments specified; use today's date
    [] -> do
      now <- getCurrentTime
      let (_, m, d) = C.toGregorian . utctDay $ now
      pure (m, d)
    -- Two arguments specified, interpret as month and date
    [m, d] -> do
      case (TR.decimal (T.pack m), TR.decimal (T.pack d)) of
        (Right (m', ""), Right (d', "")) -> pure (m', d')
        _failedParse -> errorInvalidArgs
    _anyOtherLength -> errorInvalidArgs

{-# ANN getNextDropInFromFileContents ("HLint: ignore" :: String) #-}
-- HLint doesn't like that I use `pure . tail` instead of using `<&>`, but it's
-- way cleaner like this because it fits nicely into the monadic pipeline.
getNextDropInFromFileContents :: Node -> C.MonthOfYear -> C.DayOfMonth -> Either Text DropInSession
getNextDropInFromFileContents document m d =
  getNamedSection "Schedule" document
    >>= getFirstTableFromSection
    >>= getRowsFromTable
    >>= mapM getEntriesFromRow
    >>= pure . tail -- Drop the header row.
    >>= pure . rights . map parseEntry -- pure . rights . map instead of mapM allows us to ignore rows that don't parse correctly
    >>= getNextDropInFromList m d

-- | Entry point
main :: IO ()
main = do
  (m, d) <- parseArgs

  -- Parse file contents
  contents <-
    commonmarkToNode [] [extStrikethrough, extTable, extTaskList] . T.pack <$> readFile (diFile config)

  case getNextDropInFromFileContents contents m d of
    Left err -> error (T.unpack err)
    Right t -> generateWebpage m d t
