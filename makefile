
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/src/seri --io \
		--include src/sri \
		-m Seri.SMT.Tests.Bit.main \
		-f src/sri/Seri/SMT/Tests/Bit.sri

clean:
	rm -rf build/src


