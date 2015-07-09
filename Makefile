SHELL=/bin/bash

wc:
	find thentos-core/src -name '*.hs' | xargs wc
	find thentos-core/exec -name '*.hs' | xargs wc
	find thentos-tests/{tests,bench} -name '*.hs' | xargs wc
	find thentos-adhocracy/src -name '*.hs' | xargs wc
	find thentos-adhocracy/exec -name '*.hs' | xargs wc
	find thentos-adhocracy/{tests,bench} -name '*.hs' | xargs wc
	find services/helloworld/src -name '*.hs' | xargs wc

clean:
	find thentos-*/ -name '*~' -exec rm -f {} \;
	find thentos-*/ -name '*.o' -exec rm -f {} \;
	find thentos-*/ -name '*.hi' -exec rm -f {} \;
	find thentos-*/ -name '*.dyn_o' -exec rm -f {} \;
	find thentos-*/ -name '*.dyn_hi' -exec rm -f {} \;
