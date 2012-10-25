
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/src/dist/build/seri/seri --type \
		-s Yices2 \
		-d foo.dbg \
		--include src \
		-m Seri.Lib.Tests.testallio \
		-f src/Seri/Lib/LayoutTest.sri

clean:
	rm -rf build


