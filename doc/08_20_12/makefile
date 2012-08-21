
images := \
	input1 input2\
	current1 current2 current3 current4\
	current5 current6 current7 current8\
	parser1 parser2 parser3 parser4 parser5 parser6 parser7 parser8\
	isvalid1 isvalid2 isvalid3 isvalid4 isvalid5\
	isvalid6 isvalid7 isvalid8 isvalid9 isvalid10\
   	main1 main2 main3\
   	bitcell1 bitcell2 bitcell3 bitcell4 bitcell5 bitcell6 bitcell7\
   	allq1 allq2 allq3 allq4

images_pdf := $(patsubst %,build/%.pdf,$(images))

build/pres.xx: src/seri.tex $(images_pdf)
	mkdir -p build
	cp -l src/seri.tex build/
	cd build ; pdflatex -halt-on-error seri.tex

build/%.pdf: src/%.svg
	inkscape -d 600 -A $@ $<


