# Development how-tos

This document describes some of our development practices. Please help it
grow!

## Setup

We currently use ghc 7.8.x, where x >= 4. ghc 7.10 is not yet supported.

If your package manager doesn't have a suitable ghc version, you can
download it manually from https://www.haskell.org/ghc/.

Recent versions of cabal and hlint are also required. Provided you already
have a (possibly older) cabal version installed, you can get them via:

```bash
cabal install cabal-install hlint
```

Afterwards follow the instructions from the README file to initialize a
cabal sandbox and install Thentos in it.

To run the complete test suite, you also need to download the latest
`selenium-server-standalone-x.x.x.jar` from
http://selenium-release.storage.googleapis.com/index.html and place it in
the `misc/selenium` directory. Additionally, create a log directory for selenium:

```bash
mkdir misc/selenium/log
```

## Running Thentos

To run Thentos locally, type

```bash
cabal run thentos
```

If there are no errors, the frontend will start on http://localhost:7002/
and talk to the backend running on http://localhost:7001/.

## Running the test suite

To run the complete test suite, you first have to start selenium:

```bash
cd misc/selenium && make
```

Then start the test suite (in a different shell window):

```bash
cabal test
```

If you just want to run the backend tests, you don't need selenium. Just
type:

```bash
cabal test --test-options="--skip selenium"
```

If you just want to run a specific test or set of tests, you can specify
their name (from the `describe` clause) via `--match`:

```bash
cabal test --show-details=always --test-options="--match XXX"
```

`--show-details=always` is useful to see which tests were actually
executed.

## Deleting the database

Sometimes Thentos will refuse to start because the data structures stored
in the DB no longer correspond to those in the compiled program. Our
current "migration process" for that problem is simple: Just delete the DB!

```bash
rm -rf .acid-state
```

## Adding new dependencies

If you need a new dependency (not yet listed in `thentos.cabal`), first add
it to `thentos.cabal` in the appropriate `build-depends` section(s). Please
specify a suitable minimum version (`>=`).

If everything works as it should, regenerate the `cabal.config` file that
freezes the exact versions of all libraries we use:

```bash
cabal freeze --enable-tests
```

## Creating branches and pull requests

Committing any changes directly on master is **not** allowed (except
possibly for trivial changes such as typo fixes in the documentation).
Instead, create a branch and later turn that branch into a pull request
(PR) on GitHub.

For branch names we use the following schema:
`year-month-xx-branch-description`. xx is an abbreviation identifying the
developer (that means you!), typically the initials of first name + last
name. For example, `2015-06-cs-doc-howtos` is a branch created by Christian
Siefkes in June 2015 about "doc-howtos".

If you're happy with your branch, push it to GitHub (if you didn't already)
and create a PR via the web interface. On GitHub, Travis automatically
checks all PRs for possible problems. It's a good idea to ensure that there
are none before creating the PR!

To do so, check that neither cabal nor hlint has anything to complain
about:

```bash
cabal install --ghc-options=-Werror
make hlint
```

Also run the test suite.

After creating the PR, ping somebody (e.g. Matthias) to review it.

If you are still or again working on a PR, or if you are currently
reviewing it, assign it to yourself so that others know what's going it.
