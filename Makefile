SHELL=/bin/bash

.phony:

wc: .phony
	find thentos-core/{src,exec}            -name '*.hs' | xargs wc
	find thentos-tests/{tests,bench}        -name '*.hs' | xargs wc
	find thentos-adhocracy/{src,exec,tests} -name '*.hs' | xargs wc
	find services/helloworld/src            -name '*.hs' | xargs wc

clean: .phony
	find thentos-*/ -name '*~' -exec rm -f {} \;
	find thentos-*/ -name '*.o' -exec rm -f {} \;
	find thentos-*/ -name '*.hi' -exec rm -f {} \;
	find thentos-*/ -name '*.dyn_o' -exec rm -f {} \;
	find thentos-*/ -name '*.dyn_hi' -exec rm -f {} \;


# weed out dead library dependencies.

build-packunused: .phony
	cabal get packunused || (echo "rm stale copy of packunused?"; false)
	cd packunused-* && \
	cabal sandbox init --sandbox=../.cabal-sandbox && \
	cabal install packunused --constraint="Cabal==`cabal --version | perl -ne '/using version (.*) of the Cabal library/ && print $$1'`"

%.packunused: .phony
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

packunused: .phony thentos-core.packunused thentos-tests.packunused thentos-adhocracy.packunused


# hlint

hlint: .phony thentos-core.hlint thentos-tests.hlint thentos-adhocracy.hlint

%.hlint: .phony
	cd $* && make hlint


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

prepare-repl: .phony
	echo $(SENSEI_ARGS)
	cabal sandbox hc-pkg -- unregister --force thentos-adhocracy || true
	cabal sandbox hc-pkg -- unregister --force thentos-tests || true
	cabal sandbox hc-pkg -- unregister --force thentos-core || true
	cd thentos-adhocracy && cabal clean
	cd thentos-tests && cabal clean
	cd thentos-core && cabal clean

SOURCE_PATHS=-i./thentos-core/src/ -i./thentos-tests/src/ -i./thentos-tests/tests/ -i./thentos-adhocracy/src/ -i./thentos-adhocracy/tests/

sensei: sensei-tests
sensei-tests: ./thentos-tests/tests/Spec.hs.sensei
sensei-adhocracy: ./thentos-adhocracy/tests/Spec.hs.sensei

%.sensei: .phony prepare-repl
	cabal exec -- sensei $(SOURCE_PATHS) -optP-DDEVELOPMENT -ignore-dot-ghci $* $(SENSEI_ARGS)

seito: .phony
	sleep 0.2 && seito

repl: ./thentos-core/src/Thentos.hs.repl

%.repl: .phony prepare-repl
	cabal exec -- ghci $(SOURCE_PATHS) -optP-DDEVELOPMENT -ignore-dot-ghci $*


# scratch
# (FIXME: is anybody still using this?  remove?)

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
