
all:
	tclsh8.5 tclmk/make.tcl

perf:
	./build/src/dist/build/seriq2/seriq2 -i src -m Seri.SMT.Tests.BCL.main src/Seri/SMT/Tests/BCL.sri


clean:
	rm -rf build


