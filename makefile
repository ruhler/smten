
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/src/dist/build/seri/seri --type \
		-s Yices2 \
		-d foo.dbg \
		--include src \
		-f Foo.sri

clean:
	rm -rf build


