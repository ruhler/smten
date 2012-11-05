
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/seri-bin/seri --io \
		--include seri/sri \
		-m Seri.SMT.Tests.Scoped.main \
		-f seri/sri/Seri/SMT/Tests/Scoped.sri

clean:
	rm -rf build/seri-smt build/seri build/seri-bin build/test


