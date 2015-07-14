SHELL=/bin/bash
HLINT=hlint

hlint:
	$(HLINT) --version
	find src exec -name '*.hs' | xargs $(HLINT)

wc:
	make -C .. wc

clean:
	make -C .. clean

dist-clean: clean
	cabal clean

show-splices:
	cabal install -j1 --ghc-options="-fforce-recomp -ddump-splices"

freeze:
	@cabal freeze --shadow-installed-packages  --enable-test --enable-bench\
	  || ( echo -e "\n\nthere is a neat trick that may help you here:"\
	     ; echo -e "cut&paste cabal.config to the existing dependencies"\
	     ; echo -e "in lib target in thentos.cabal, then try again."\
	     ; echo -e "this may not yield the most up-to-date solution, but"\
	     ; echo -e "it is an easy way to get all dependencies of new libs"\
	     ; echo -e "listed in cabal.config.")
