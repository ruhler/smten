
all:
	tclsh8.5 tclmk/make.tcl

perf:
	./build/src/dist/build/seriq/seriq -d bcl.dbg -i src -m Seri.SMT.Tests.BCL.main src/Seri/SMT/Tests/BCL.sri

perf2:
	./build/src/dist/build/seriq2/seriq2 -d bcl.2.dbg -i src -m Seri.SMT.Tests.BCL.main src/Seri/SMT/Tests/BCL.sri +RTS -H4g

clean:
	rm -rf build


