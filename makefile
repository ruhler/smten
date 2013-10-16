
all:
	tclsh8.5 tclmk/make.tcl

userinstall:
	tclsh8.5 tclmk/make.tcl
	cd build/smten ; cabal install --builddir ../build/smten-$(USER) --force-reinstalls
	cd build/smten-base ; cabal install --builddir ../smten-base-$(USER) --force-reinstalls
	cd build/smten-lib ; cabal install --builddir ../smten-lib-$(USER) --force-reinstalls
	cd build/smten-yices1 ; cabal install --builddir ../smten-yices1-$(USER) --force-reinstalls
	cd build/smten-stp ; cabal install --builddir ../smten-stp-$(USER) --force-reinstalls
	cd build/smten-minisat ; cabal install --builddir ../smten-minisat-$(USER) --force-reinstalls

userinstall2:
	cd build/smten ; cabal install --builddir ../build/smten-$(USER) --force-reinstalls
	cd build/smten-base ; cabal install --builddir ../smten-base-$(USER) --force-reinstalls
	cd build/smten-lib ; cabal install --builddir ../smten-lib-$(USER) --force-reinstalls
	cd build/smten-yices1 ; cabal install --builddir ../smten-yices1-$(USER) --force-reinstalls
	cd build/smten-stp ; cabal install --builddir ../smten-stp-$(USER) --force-reinstalls
	cd build/smten-minisat ; cabal install --builddir ../smten-minisat-$(USER) --force-reinstalls

clean:
	rm -rf build/smten*

fullclean:
	rm -rf build/*


