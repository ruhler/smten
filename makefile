
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/seri-bin/seri --io \
		--include seri/sri \
		-m Seri.SATLIB.SAT.main \
		-f seri/sri/Seri/SATLIB/SAT.sri +RTS -p

clean:
	rm -rf build/seri-smt build/seri build/seri-bin build/test


