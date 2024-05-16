# REG drop-in session notifications

The contents of this directory allow custom drop-in session notifications to be posted in Slack.

An overview of the steps is as follows:

 - The `dropin.yaml` file specifies a GitHub Action which runs the Haskell code in this directory.
    - The action is run every Tuesday and Wednesday mornings (so that the page title can be updated to read `Today's Drop-In Session`), as well as every time the repository wiki is updated (i.e. whenever the schedule is changed).

 - The Haskell code (`dropin.cabal` and `dropin.hs`):
    - assumes that the repository wiki can be accessed at the `.github/workflows/dropin/wiki` directory
    - parses the drop-in session Markdown file to obtain the dates, people involved in, and status of each drop-in session
    - identifies the next drop-in session and generates a HTML file in the `.github/workflows/dropin/web` directory containing info about it.

 - This HTML file is never committed to the main branch, but is instead placed on [the `gh-pages` branch](https://github.com/alan-turing-institute/infrastructure-drop-ins/tree/gh-pages).
    - The HTML file contains `meta` tags which are parsed by Slack into a nice format.
    - It does not otherwise contain any actual content; instead, it is configured to redirect to the schedule (in the wiki).

 - In the repository settings, GitHub Pages is set up to publish from the `gh-pages` branch.
    - This means that the HTML file above can be accessed at https://alan-turing-institute.github.io/infrastructure-drop-ins/drop-in.

 - A Slack workflow is set up to post a message every Tuesday morning, pointing to this link.

## To test locally

- Install GHC 9.6.3 and Cabal (the Haskell build tool).
  On a Mac, the easiest way is to use [`ghcup`](https://www.haskell.org/ghcup/).
  Once you've downloaded `ghcup`, run:

      ghcup install ghc 9.6.3
      ghcup install cabal 3.10.1.0

  The version of GHC is important (if you use a different version, it will fail to compile, because the dependencies are pinned to a specific version).
  However, the version of Cabal isn't important.

- Clone the repo:

      git clone git@github.com:alan-turing-institute/infrastructure-drop-ins.git

- Clone the wiki into the appropriate directory:

      cd infrastructure-drop-ins/.github/workflows/dropin
      git clone git@github.com:alan-turing-institute/infrastructure-drop-ins.wiki.git wiki

- Run the programme:

      cabal run

## I want to...

**...stop the Slack notifications / edit the message**:

- Edit the workflow on Slack.
  Open the list of members in any channel, then click on 'Integrations' > 'Add a workflow'.

**...change the Zoom link, or the physical meeting room that will be displayed**:

- Edit the configuration variables near the top of the Haskell source code (`.github/workflows/dropin/dropin.hs`).
  Then rerun the workflow from GitHub ('Actions' tab > `.github/workflows/dropin.yaml` > 'Run workflow') to make sure that the changes are picked up.

**...change the speaker or topic that will be displayed**:

- Edit the wiki page. The changes to the wiki will be automatically picked up by the GitHub Action.

**...do something else not listed here**:

- If anything about this is unclear (or if the displayed results are not correct), please message Jonathan Yong on Slack!
