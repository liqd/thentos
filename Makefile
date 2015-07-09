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

%.packunused:
	@echo
	@echo NOTE: run 'cabal install packunused' and make sure it is in your
	@echo       PATH if this fails.
	@echo
	@echo WARNING: packunused on hspec-discover tests cannot see Spec modules
	@echo          and will issue spurious warnings about unused libs!
	@echo

	cd $* && \
	packunused --help >/dev/null && \
	cabal clean && \
	rm -f *.imports && \
	cabal configure -O0 --disable-library-profiling --enable-test --enable-bench && \
	cabal build --ghc-option=-ddump-minimal-imports && \
	packunused

packunused: thentos-core.packunused thentos-tests.packunused thentos-adhocracy.packunused

%.hlint:
	cd $* && make hlint

hlint: thentos-core.hlint
