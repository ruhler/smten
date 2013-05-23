
all:
	tclsh8.5 tclmk/make.tcl

userinstall:
	cd smten ; cabal install --builddir ../build/smten-$(USER) --force-reinstalls
	cd smten-smt ; cabal install --builddir ../build/smten-smt-$(USER) --force-reinstalls
	cd smten-bin ; cabal install --builddir ../build/smten-bin-$(USER) --force-reinstalls

test: dosmten doghc
	./build/test/Smten_Tests_All

dosmten:
	./build/home/.cabal/bin/smten --haskellf \
		--include smten/share/lib \
		--file smten/share/lib/Smten/Tests/All.smtn \
		--hsdir build/test

doghc: build/test/Smten/Lib/Smten/Tests/All.hs
	ghc -main-is Smten.Lib.Smten.Tests.All.main__ -ibuild/test -o build/test/Smten_Tests_All $<

testph:
	./build/home/.cabal/bin/smten --phases \
		--include smten/share/lib \
		--file smten/share/lib/Smten/Tests/All.smtn \
		-o build/test/All

clean:
	rm -rf build/smten-smt build/smten build/smten-bin build/test


