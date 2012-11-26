
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/seri-bin/seri --haskellf \
		--include seri/sri \
<<<<<<< HEAD
		-m main \
		-f seri/sri/Seri/SMT/Tests/Core.sri > foo.hs
	HOME=build/home ghc -fno-warn-overlapping-patterns \
		-fno-warn-missing-fields \
		-prof -auto-all -rtsopts \
		-o foo foo.hs
	./foo +RTS -p
=======
		-m Seri.SATLIB.SAT.main \
		-f seri/sri/Seri/SATLIB/SAT.sri +RTS -p
>>>>>>> master

clean:
	rm -rf build/seri-smt build/seri build/seri-bin build/test


