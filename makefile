
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/src/dist/build/seriq2/seriq2 \
		-d foo.dbg -i src \
		-m Seri.SMT.Tests.Sketch2QBF.main \
		src/Seri/SMT/Tests/Sketch2QBF.sri


clean:
	rm -rf build


