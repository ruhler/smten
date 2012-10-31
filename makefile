
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/src/seri --io \
		--include src/sri \
		-m Seri.SMT.Tests.Core.main \
		-f src/sri/Seri/SMT/Tests/Core.sri

clean:
	rm -rf build/src


