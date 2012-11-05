
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/seri-bin/seri --io \
		--include seri/sri \
		-m Seri.SMT.Tests.Sudoku.main \
		-f seri/sri/Seri/SMT/Tests/Sudoku.sri

clean:
	rm -rf build/seri-smt build/seri build/seri-bin build/test


