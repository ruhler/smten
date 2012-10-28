
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/src/seri --io \
		--include src \
		-m Seri.SMT.Tests.HelloWorld.main \
		-f src/Seri/SMT/Tests/HelloWorld.sri

clean:
	rm -rf build/src


