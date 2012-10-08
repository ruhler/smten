
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/src/dist/build/seriq2/seriq2 \
		-i src \
		-m Seri.SMT.Tests.BCL3.main \
		src/Seri/SMT/Tests/BCL3.sri +RTS -p


clean:
	rm -rf build


