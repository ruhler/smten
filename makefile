
all:
	tclsh8.5 tclmk/make.tcl

userinstall:
	cd smten-runtime ; cabal install --builddir ../build/smten-runtime-$(USER) --force-reinstalls
	cd smten-plugin ; cabal install --builddir ../build/smten-plugin-$(USER) --force-reinstalls

clean:
	rm -rf build/smten* build/test


