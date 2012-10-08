
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/src/dist/build/seriq2/seriq2 \
		-i src \
		-m Seri.SMT.Tests.Sudoku.main \
		src/Seri/SMT/Tests/Sudoku.sri +RTS -p


clean:
	rm -rf build


