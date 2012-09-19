
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/src/dist/build/seriq1/seriq1 \
		-d foo.dbg -i src \
		-m Seri.SMT.Tests.SquaresRaw.main \
		src/Seri/SMT/Tests/SquaresRaw.sri


clean:
	rm -rf build


