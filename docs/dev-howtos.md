# Development how-tos

This document describes some of our development practices. Please help it
grow!

## Setup

Please follow the instructions from the README file to initialize a cabal
sandbox and install Thentos in it. For development, you'll also need recent
versions of cabal and hlint. Provided you already have a (possibly older)
cabal version installed, you can get them via:

```shell
$ cabal install cabal-install hlint
```

To run the complete test suite, you need to download the latest
`selenium-server-standalone-x.x.x.jar` from
http://selenium-release.storage.googleapis.com/index.html and place it in
the `misc/selenium` directory. Additionally, create a log directory for
selenium:

```shell
$ mkdir misc/selenium/log
```

You'll also need the submodules that we use.

```shell
git submodule update --init
```
And (assuming you're using sandboxes, and not stack):

```shell
cabal sandbox add-source submodules servant/servant
cabal sandbox add-source submodules servant/servant-server
cabal sandbox add-source submodules servant/servant-client
cabal sandbox add-source submodules servant/servant-docs
cabal sandbox add-source submodules servant/servant-jquery
cabal sandbox add-source submodules servant/servant-blaze
cabal sandbox add-source submodules servant/servant-lucid
```

## Running Thentos

To run Thentos locally, type

```shell
$ cabal run thentos
```

If there are no errors, the frontend will start on http://localhost:7002/
and talk to the backend running on http://localhost:7001/.

Take a look at the configuration file `devel.config` if you want to change
this.

## Running the test suite

To run the complete test suite, you first have to start selenium:

```shell
$ cd misc/selenium && make
```

Then start the test suite (in a different shell window):

```shell
$ cabal test
```

If you just want to run the backend tests, you don't need selenium. Just
type:

```shell
$ cabal test --test-options="--skip selenium"
```

If you just want to run a specific test or set of tests, you can specify
their name (from the `describe` clause) via `--match`:

```shell
$ cabal test --show-details=always --test-options="--match XXX"
```

`--show-details=always` is useful to see which tests were actually
executed.

# Running benchmarks

If you want to run benchmarks, you need to add a patched version of `pronk` to
your cabal sandbox:

```shell
$ git clone https://github.com/liqd/pronk -b thentos-patches
$ cabal sandbox add-source pronk
```

To start the benchmarks:

```shell
$ cabal install --enable-bench
$ cabal bench  # requires thentos to be running in another shell
```


## Updating or deleting the database

If the database state type changes,
http://hackage.haskell.org/package/safecopy is used to migrate deprecated
existing serializations from disk transparently.

There should be tests that make sure nobody breaks this rule, but currently
there aren't.  Instead, the current "migration process" is simple: Just
delete the DB!

```shell
$ rm -rf .acid-state
```

## Adding new dependencies

If you need a new dependency (not yet listed in `thentos.cabal`), first add
it to `thentos.cabal` in the appropriate `build-depends` section(s). Please
specify a suitable minimum (`>=`) and maximum version. A good rule of thumb
is to set the lower bound to whatever is most recent at the time of
creating the dependency, and the upper bound to `<a.(b+1)`, where
`a.b.c...` is the lower bound.

If everything works as it should, regenerate the `cabal.config` file that
freezes the exact versions of all libraries we use:

```shell
$ cabal freeze --enable-tests
```

Check `git diff cabal.config` to see if everything went as expected.

## Creating branches and pull requests

Committing any changes directly on master is **not** allowed. Instead,
create a branch and later turn that branch into a pull request (PR) on
GitHub.

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

```shell
$ cabal install --ghc-options=-Werror
$ make hlint
```

Also run the test suite.

After creating the PR, ping somebody (e.g. Matthias) to review it.

If you are still or again working on a PR, or if you are currently
reviewing it, assign it to yourself so that others know what's going it.
