
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/src/seri --query \
		-s Yices1 \
		-d foo.dbg \
		--include src \
		-m Seri.SMT.Tests.Query1.main \
		-f src/Seri/SMT/Tests/Query1.sri

clean:
	rm -rf build/src


