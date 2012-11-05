
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/seri-bin/seri --io \
		--include seri/sri \
		-m Seri.Tests.Basic.testallio \
		-f seri/sri/Seri/Tests/Basic.sri

clean:
	rm -rf build/seri-smt build/seri build/seri-bin build/test


