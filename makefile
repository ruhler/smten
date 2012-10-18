
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/src/dist/build/seri/seri --query \
		-s Yices2 \
		-d foo.dbg \
		--include src \
		-m Seri.SMT.Tests.Sudoku3.main \
		-f src/Seri/SMT/Tests/Sudoku3.sri

clean:
	rm -rf build


