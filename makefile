
all:
	tclsh8.5 tclmk/make.tcl

test:
	./build/src/dist/build/serie/serie \
		-o build/src/Seri/SMT/Tests/Squares2/Squares.out \
		-i build/src \
		-m Seri.SMT.Tests.Squares2.Squares.main \
		build/src/Seri/SMT/Tests/Squares2/Squares.sri

#		-d build/src/Seri/SMT/Tests/Squares/Squares2.2.dbg -i build/src \
#		-m Seri.SMT.Tests.Squares2.Squares.main \
#		build/src/Seri/SMT/Tests/Squares2/Squares.sri \
#		> build/src/Seri/SMT/Tests/Squares2/Squares.out


clean:
	rm -rf build


