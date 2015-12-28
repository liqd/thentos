# Development how-tos

This document describes some of our development practices. Please help it
grow!

## Source comment keywords

We use the following greppable keywords in the source code with the
resp.  specific meaning:

- *TODO*: note to self to developer currently working on this piece of
   code.  to be fixed before merge.  should never appear in master.
- *FIXME*: this code is not elegant, but it is believed to work.
   FIXME comments may appear on master and can be fixed, removed, or
   ignored indefinitely without causing production issues.
- *FUTUREWORK*: a less urgent sibling of *FIXME*.
- *UPSTREAM*: could benefit from some work on the libraries we depend
   on.  one common example for this are: `*.Missing` modules.

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
misc/thentos-install.hs
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

If you want to run benchmarks, you need to add a patched version of
`pronk` from the git submodules to your cabal sandbox:

```shell
$ cd thentos-tests
$ cabal sandbox add-source ../submodules/pronk
```

To start the benchmarks:

```shell
$ cabal install --enable-bench
$ cabal bench  # requires thentos to be running in another shell
```

## Updating or deleting the database

Changes to the database schema are implemented in three stages:

1. change the data types in haskell code
2. change ./schema/schema.sql
3. write a script that migrates a database of the before-schema to the now-schema.

All this needs to be done manually.  For version control, the
migration script should be named `migrate-<hash1>-<hash2>.sql`, where
the `hash1` and `hash2` are commits that contain ./schema/schema.sql
in the before-version and the now-version, resp.

## purescript frontend

`/thentos-purescript` contains frontend code to be run in the browser.
It can be distributed via the `thentos-core` module
`Thentos.Backend.Api.Purescript` (see test suite for examples).
`./misc/thentos-install.hs` should build everything for you.  To watch
it, make sure that the `purescript` field in `devel.config` is aimed
in the right direction and open browser at the backend under `/js/`.

Continuous compilation (aka `--watch`) is supported: Simply run
`/thentos-purescript/build.sh watch`.

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

## Updating the documentation

If you have write access to git:github.com/liqd/thentos, you can
update the documentation linked from the README as follows.  (If you
do not, you can still use this to generate the documentation, but you
can't publish it.)

```shell
$ ./misc/thentos-install.hs -c "--enable-documentation --force-reinstalls"
$ cabal exec -- ghc --make -main-is Doc misc/build-docs/Doc.hs
$ git checkout gh-pages
```

Generate servant docs and client source code:

```shell
$ misc/build-docs/Doc
$ git add gh-pages/servant-docs
```

Then go to `gh-pages/servant-docs/index.html`, run the shell command
in the html comment, and overwrite the body with the output.

Copy haddocks:

```shell
$ git rm -r gh-pages/haddock/doc/
$ mkdir -p gh-pages/haddock/
$ cp -r .cabal-sandbox/share/doc/ gh-pages/haddock/
$ git add gh-pages/haddock/
```

Links in haddock files generated into the sandbox are absolute.  Workaround:

```shell
find ~/thentos/gh-pages/haddock/ -type f -exec \
    perl -i -pe 's!file:///'$THENTOS_ROOT'/.cabal-sandbox/share/!/thentos/gh-pages/haddock/!g' {} \;
find ~/thentos/gh-pages/haddock/ -type f -exec \
    perl -i -pe 's!file:///[^"]*"!"/!g' {} \;
```

Finally, the
[SourceGraph](https://hackage.haskell.org/package/SourceGraph/) docs.
On `master`:

```shell
cabal install SourceGraph
./.cabal-sandbox/bin/SourceGraph thentos-core/src/Thentos.hs
./.cabal-sandbox/bin/SourceGraph thentos-adhocracy/src/Thentos/Adhocracy3.hs
```

Then, on `gh-pages`:

```shell
git rm -r gh-pages/SourceGraph
mkdir -p gh-pages/SourceGraph
mv thentos-core/src/SourceGraph gh-pages/SourceGraph/thentos-core
mv thentos-adhocracy/src/Thentos/SourceGraph gh-pages/SourceGraph/thentos-adhocracy
git add gh-pages/SourceGraph
```
