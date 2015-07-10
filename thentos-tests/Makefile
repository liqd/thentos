SHELL=/bin/bash
HLINT=hlint

hlint:
	$(HLINT) --version
	find src tests bench -name '*.hs' | xargs $(HLINT)
