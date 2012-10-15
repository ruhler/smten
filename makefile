
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/src/dist/build/serio/serio \
		-i src \
		-m Seri.IO.Tests.Simple.main \
		src/Seri/IO/Tests/Simple.sri +RTS -p


clean:
	rm -rf build


