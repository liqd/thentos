SHELL=/bin/bash

lint:
	find src tests -name '*.hs' | xargs hlint

wc:
	find src -name '*.hs' | xargs wc
	find tests -name '*.hs' | xargs wc
	find services/helloworld/src -name '*.hs' | xargs wc

clean:
	find . -name '*~' -exec rm -f {} \;

dist-clean: clean
	cabal clean

packunused:
	packunused --help >/dev/null  # run `cabal install packunused` and make sure it is in your PATH if this fails
	cabal clean
	rm -f *.imports
	cabal configure -O0 --disable-library-profiling
	cabal build --ghc-option=-ddump-minimal-imports
	packunused

show-splices:
	cabal install -j1 --ghc-options="-fforce-recomp -ddump-splices"
