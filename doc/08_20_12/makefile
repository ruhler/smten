
images := 
images_pdf := $(patsubst %,build/%.pdf,$(images))

build/pres.xx: src/seri.tex $(images_pdf)
	mkdir -p build
	cp -l src/seri.tex build/
	cd build ; pdflatex -halt-on-error seri.tex

build/%.pdf: fig/%.svg
	inkscape -d 600 -A $@ $<


