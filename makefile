
all:
	tclsh8.5 tclmk/make.tcl

userinstall:
	cd smten-plugin ; cabal install --builddir ../build/smten-plugin-$(USER) --force-reinstalls
	cd build/smten-base ; cabal install --builddir ../smten-base-$(USER) --force-reinstalls
	cd build/smten-lib ; cabal install --builddir ../smten-lib-$(USER) --force-reinstalls
	cd build/smten-yices2 ; cabal install --builddir ../smten-yices2-$(USER) --force-reinstalls

clean:
	rm -rf build/smten* build/test


