
all:
	tclsh8.5 tclmk/make.tcl

userinstall:
	tclsh8.5 tclmk/make.tcl
	tclsh8.5 tclmk/install.tcl

userinstall2:
	tclsh8.5 tclmk/install.tcl

clean:
	rm -rf build/smten* build/home/.cabal/lib/*/smten-*

fullclean:
	rm -rf build/*


