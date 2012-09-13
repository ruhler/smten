
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/src/dist/build/seriq2/seriq2 \
		-d build/src/Seri/SMT/Tests/Squares/Squares.2.dbg -i build/src \
		-m Seri.SMT.Tests.Squares.Squares.main \
		build/src/Seri/SMT/Tests/Squares/Squares.sri \
		> build/src/Seri/SMT/Tests/Squares/Squares.out


clean:
	rm -rf build


