
all:
	tclsh8.5 tclmk/make.tcl

perf:
	./build/src/dist/build/seriq2/seriq2 -i src -m Seri.SMT.Tests.BCL2.main src/Seri/SMT/Tests/BCL2.sri


clean:
	rm -rf build


