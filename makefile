
all:
	tclsh8.5 tclmk/make.tcl

userinstall:
	cd smten ; cabal install --builddir ../build/smten-$(USER) --force-reinstalls -O2
	cd smten-smt ; cabal install --builddir ../build/smten-smt-$(USER) --force-reinstalls -O2
	cd smten-bin ; cabal install --builddir ../build/smten-bin-$(USER) --force-reinstalls -O2

testio:
	./build/home/.cabal/bin/smten --io \
		--include smten/share/lib \
		--file smten/share/lib/Smten/Tests/All.smtn \
		--main-is Smten.Tests.All.main

testph:
	./build/home/.cabal/bin/smten --phases \
		--include smten/share/lib \
		--file smten/share/lib/Smten/Tests/All.smtn \
		-o build/test/phases

clean:
	rm -rf build/smten-smt build/smten build/smten-bin build/test


