
all:
	tclsh8.5 tclmk/make.tcl

userinstall:
	cd smten ; cabal install --builddir ../build/smten-$(USER) --force-reinstalls
	cd smten-smt ; cabal install --builddir ../build/smten-smt-$(USER) --force-reinstalls
	cd smten-bin ; cabal install --builddir ../build/smten-bin-$(USER) --force-reinstalls

testio:
	./build/home/.cabal/bin/smten --io \
		--include smten/smtn \
		--file smten/smtn/Smten/SMT/Tests/Nest.smtn \
		--main-is Smten.SMT.Tests.Nest.main

testsugar:
	./build/home/.cabal/bin/smten --desugar \
		--include smten/smtn \
		--file smten/smtn/Smten/Tests/Concrete.smtn \
		-o desugared

clean:
	rm -rf build/smten-smt build/smten build/smten-bin build/test


