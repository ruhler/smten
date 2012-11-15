
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/seri-bin/seri --io \
		--include seri/sri \
		-m Seri.TSPLIB.HCP.HCP.main \
		-f seri/sri/Seri/TSPLIB/HCP/HCP.sri

clean:
	rm -rf build/seri-smt build/seri build/seri-bin build/test


