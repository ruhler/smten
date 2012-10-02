
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/src/dist/build/seriq2/seriq2 \
		-d foo.dbg -i src \
		-m Seri.SMT.Tests.Sudoku2.main \
		src/Seri/SMT/Tests/Sudoku2.sri


clean:
	rm -rf build


