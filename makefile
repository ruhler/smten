
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/seri-bin/seri --haskellf \
		--include seri/sri \
		-m main \
		-f seri/sri/Seri/SMT/Tests/Datatype.sri > foo.hs
	HOME=build/home ghc -fno-warn-overlapping-patterns \
		-fno-warn-missing-fields \
		-prof -auto-all -rtsopts \
		-o foo foo.hs
	./foo +RTS -p

clean:
	rm -rf build/seri-smt build/seri build/seri-bin build/test


