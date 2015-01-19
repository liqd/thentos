SHELL=/bin/bash

# this is a filthy hack, and ought to be replaced by something shakey.

serve:
	cabal install --disable-documentation && ./.cabal-sandbox/bin/thentos -r

lint:
	find src tests -name '*.hs' | xargs hlint  # --report
#	rm -f report.html

wc:
	find src -name '*.hs' | xargs wc
	find tests -name '*.hs' | xargs wc
#	find src -name '*.hs' | xargs sloccount

clean:
	find . -name '*~' -exec rm -f {} \;

dist-clean: clean
	rm -rf dist

show-splices:
	cabal install -j1 --ghc-options="-fforce-recomp -ddump-splices"

# activate a copy of the cabal file with private changes.
cabal-private:
	cp thentos.cabal{-private,}

# activate a the cabal from current repo state.
cabal-public:
	git checkout -- thentos.cabal
