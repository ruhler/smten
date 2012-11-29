
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/seri-bin/seri --haskellf \
		--include seri/sri \
		-m main \
		-f seri/sri/Seri/Tests/Basic.sri > foo.hs
	HOME=build/home ghc -O2 -fno-warn-overlapping-patterns \
		-fno-warn-missing-fields \
		-main-is __main \
		-prof -auto-all -rtsopts \
		-o foo foo.hs
	./foo +RTS -p

testio:
	./build/seri-bin/seri --io \
		--include seri/sri \
		-m Seri.SMT.Tests.Core.main \
		-f seri/sri/Seri/SMT/Tests/Core.sri +RTS -p

clean:
	rm -rf build/seri-smt build/seri build/seri-bin build/test


