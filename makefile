
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/seri-bin/seri --type \
		--include seri/sri \
		-m Main.main \
		-f foo.sri +RTS -p

clean:
	rm -rf build/seri-smt build/seri build/seri-bin build/test


