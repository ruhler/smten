
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/src/dist/build/serie/serie \
		-o foo.out -i src \
		-m Seri.SMT.Tests.SquaresRaw.main \
		src/Seri/SMT/Tests/SquaresRaw.sri


clean:
	rm -rf build


