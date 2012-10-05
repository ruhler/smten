
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/src/dist/build/seriq2/seriq2 \
		-d foo.dbg -i src \
		-m Seri.SMT.Tests.Array.main \
		src/Seri/SMT/Tests/Array.sri +RTS -K1g -p


clean:
	rm -rf build


