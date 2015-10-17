SHELL=/bin/bash

wc:
	find thentos-core/{src,exec}            -name '*.hs' | xargs wc
	find thentos-tests/{tests,bench}        -name '*.hs' | xargs wc
	find thentos-adhocracy/{src,exec,tests} -name '*.hs' | xargs wc
	find services/helloworld/src            -name '*.hs' | xargs wc

clean:
	find thentos-*/ -name '*~' -exec rm -f {} \;
	find thentos-*/ -name '*.o' -exec rm -f {} \;
	find thentos-*/ -name '*.hi' -exec rm -f {} \;
	find thentos-*/ -name '*.dyn_o' -exec rm -f {} \;
	find thentos-*/ -name '*.dyn_hi' -exec rm -f {} \;


# weed out dead library dependencies.

build-packunused:
	cabal get packunused || (echo "rm stale copy of packunused?"; false)
	cd packunused-* && \
	cabal sandbox init --sandbox=../.cabal-sandbox && \
	cabal install packunused --constraint="Cabal==`cabal --version | perl -ne '/using version (.*) of the Cabal library/ && print $$1'`"

%.packunused:
	@echo
	@echo '| make build-packunused first to ensure freshness of executable.'
	@echo '| (See https://github.com/hvr/packunused/issues/5.)'
	@echo '|'
	@echo '| WARNING: packunused on hspec-discover tests cannot see Spec modules'
	@echo '|          and will issue spurious warnings about unused libs!'
	@echo

	cd $* && \
	../.cabal-sandbox/bin/packunused --help >/dev/null && \
	cabal clean && \
	rm -f *.imports && \
	cabal configure -O0 --disable-library-profiling --enable-test --enable-bench && \
	cabal build --ghc-option=-ddump-minimal-imports && \
	../.cabal-sandbox/bin/packunused

packunused: thentos-core.packunused thentos-tests.packunused thentos-adhocracy.packunused


# hlint

%.hlint:
	cd $* && make hlint

hlint: thentos-core.hlint thentos-tests.hlint thentos-adhocracy.hlint


# sensei / seito

# ABSTRACT: a ghcid-based method for running the test suite blindingly
# fast rather than just the type checker painfully slowly.
#
# QUICK INTRO: you need to install https://github.com/hspec/sensei
# first.  run 'make sensei' in a new terminal at the beginning of your
# session and keep it running.  it will re-run the test suite every
# time something changes, or if you hit 'return'.  you can also run
# 'make seito' to print the last test run to stdout.  (this is most
# useful if you want to integrate sensei into your editor/ide.)
#
# OTHER PACKAGES: sensei does not watch other packages (for deeper
# reasons).  in order to be able to react to changes to the core from
# the test suite, these make rules drop thentos-core and thentos-test
# from the package database and add their source trees to the list of
# watched files.
#
# NOTE: if you want to work with package thentos-adhocracy, these
# rules need to be updated!
#
# OPTIMIZATION: for optimal results, you will want to invoke sensei
# with the '--match' argument.  hspec arguments can be passed to 'make
# sensei' via the SENSEI_ARGS shell variable.  see sensei and hspec
# docs for details.

sensei:
	cabal sandbox hc-pkg -- unregister --force thentos-tests
	cabal sandbox hc-pkg -- unregister --force thentos-core
	cd thentos-tests && cabal clean
	cd thentos-core && cabal clean
	cabal exec -- sensei \
	  -i./thentos-core/src/ \
	  -i./thentos-tests/src/ \
	  -i./thentos-tests/tests/ ./thentos-tests/tests/Spec.hs $(SENSEI_ARGS)

seito:
	sleep 0.2 && seito


# scratch

install2:
	cd thentos-core && \
	  cabal sandbox init --sandbox=../.cabal-sandbox && \
	  cabal install --dependencies-only --enable-tests --enable-bench
	cd thentos-tests && \
	  cabal sandbox init --sandbox=../.cabal-sandbox && \
	  cabal sandbox add-source ../thentos-core && \
	  cabal install --dependencies-only --enable-tests --enable-bench
	cd thentos-adhocracy && \
	  cabal sandbox init --sandbox=../.cabal-sandbox && \
	  cabal sandbox add-source ../thentos-core && \
	  cabal sandbox add-source ../thentos-tests && \
	  cabal install --dependencies-only --enable-tests --enable-bench

tests2:
	cd thentos-tests && \
	  cabal configure --enable-tests --ghc-options="-Werror" && cabal build && cabal test
	cd thentos-adhocracy && \
	  cabal configure --enable-tests --ghc-options="-Werror" && cabal build && cabal test
