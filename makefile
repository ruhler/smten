
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/src/dist/build/seriq/seriq \
		-d foo.dbg \
		-i src \
		-m Seri.SMT.Tests.Query1.main \
		-f src/Seri/SMT/Tests/Query1.sri


clean:
	rm -rf build


