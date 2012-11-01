
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/src/seri --io \
		--include src/sri \
		-m Seri.SMT.Tests.Sudoku2.main \
		-f src/sri/Seri/SMT/Tests/Sudoku2.sri

clean:
	rm -rf build/src


