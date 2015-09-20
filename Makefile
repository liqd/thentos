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

%.hlint:
	cd $* && make hlint

hlint: thentos-core.hlint thentos-tests.hlint thentos-adhocracy.hlint

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
