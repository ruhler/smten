
all:
	tclsh8.5 tclmk/make.tcl

userinstall:
	cd smten ; cabal install --builddir ../build/smten-$(USER) --force-reinstalls
	cd smten-smt ; cabal install --builddir ../build/smten-smt-$(USER) --force-reinstalls
	cd smten-bin ; cabal install --builddir ../build/smten-bin-$(USER) --force-reinstalls

testio:
	./build/home/.cabal/bin/smten --io \
		--include smten/share/lib \
		--file smten/share/lib/Smten/SMT/Tests/Error.smtn \
		--main-is Smten.SMT.Tests.Error.main +RTS -p

testhf:
	./build/home/.cabal/bin/smten --haskellf \
		--include smten/share/lib \
		--file smten/share/lib/Smten/SMT/Tests/Datatype.smtn \
		--main-is Smten.SMT.Tests.Datatype.main -o testh.hs
	HOME=./build/home/ ghc -o testh testh.hs -main-is __main -prof -rtsopts
	./testh

testsugar:
	./build/home/.cabal/bin/smten --desugar \
		--include smten/share/lib \
		--file smten/share/lib/Smten/SMT/Tests/Share.smtn \
		-o desugared

clean:
	rm -rf build/smten-smt build/smten build/smten-bin build/test


