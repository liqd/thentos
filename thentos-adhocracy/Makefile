SHELL=/bin/bash
HLINT=hlint

hlint:
	$(HLINT) --version
	find src exec tests -name '*.hs' | xargs $(HLINT)
