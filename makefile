
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/src/dist/build/seriq2/seriq2 \
		-d foo.dbg -i src \
		-m Seri.SMT.Tests.Squares2.Squares.main \
		src/Seri/SMT/Tests/Squares2/Squares.sri


clean:
	rm -rf build


