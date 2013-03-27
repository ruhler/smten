
all:
	tclsh8.5 tclmk/make.tcl

userinstall:
	cd smten ; cabal install --builddir ../build/smten-$(USER) --force-reinstalls
	cd smten-smt ; cabal install --builddir ../build/smten-smt-$(USER) --force-reinstalls
	cd smten-bin ; cabal install --builddir ../build/smten-bin-$(USER) --force-reinstalls

testio:
	./build/home/.cabal/bin/smten --io \
		--include smten/share/lib \
		--file smten/share/lib/Smten/SMT/Tests/Share.smtn \
		--main-is Smten.SMT.Tests.Share.main +RTS -K1g -p

testh:
	./build/home/.cabal/bin/smten --haskellf \
		--include smten/share/lib \
		--file smten/share/lib/Smten/SMT/Tests/Error.smtn \
		--main-is Smten.SMT.Tests.Error.main -o foo.hs
	HOME=`pwd`/build/home ghc -o foo foo.hs -main-is __main -prof
	./foo

testsugar:
	./build/home/.cabal/bin/smten --desugar \
		--include smten/share/lib \
		--file smten/share/lib/Smten/SMT/Tests/Datatype.smtn \
		-o desugared

clean:
	rm -rf build/smten-smt build/smten build/smten-bin build/test


