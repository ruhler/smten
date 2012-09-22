
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/src/dist/build/seriq2/seriq2 \
		-d foo.dbg -i src \
		-m Seri.SMT.Tests.Share.main \
		src/Seri/SMT/Tests/Share.sri


clean:
	rm -rf build


