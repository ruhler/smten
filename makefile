
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/src/dist/build/seriq1/seriq1 \
		-d build/src/Seri/SMT/Tests/Squares/Squares.1.dbg -i build/src \
		-m Seri.SMT.Tests.Squares.Squares.main \
		build/src/Seri/SMT/Tests/Squares/Squares.sri \
		> build/src/Seri/SMT/Tests/Squares/Squares.out


clean:
	rm -rf build


