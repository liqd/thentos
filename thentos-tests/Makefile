SHELL=/bin/bash
HLINT=hlint

test:
	cabal test --test-options="--skip selenium"

test-all:
	cabal test

hlint:
	$(HLINT) --version
	find src tests bench -name '*.hs' | xargs $(HLINT)
