SHELL=/bin/bash
HLINT=hlint

test:
	cabal test --test-options="--skip selenium"

test-all:
	cabal test

hlint:
	$(HLINT) --version
	find src tests exec -name '*.hs' | xargs $(HLINT)

wc:
	make -C .. wc

clean:
	make -C .. clean

dist-clean: clean
	cabal clean

show-splices:
	cabal install -j1 --ghc-options="-fforce-recomp -ddump-splices"

update-stackage:
#	wget https://www.stackage.org/lts/cabal.config
	rm cabal.config
	wget https://www.stackage.org/snapshot/nightly-`date +%F`/cabal.config

freeze:
	@cabal freeze --shadow-installed-packages  --enable-test --enable-bench\
	  || ( echo -e "\n\nthere is a neat trick that may help you here:"\
	     ; echo -e "cut&paste cabal.config to the existing dependencies"\
	     ; echo -e "in lib target in thentos.cabal, then try again."\
	     ; echo -e "this may not yield the most up-to-date solution, but"\
	     ; echo -e "it is an easy way to get all dependencies of new libs"\
	     ; echo -e "listed in cabal.config.")
