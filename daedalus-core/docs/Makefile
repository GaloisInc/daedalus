.PHONY: clean

core.pdf: core.tex macro.tex build
	pdflatex -output-directory build $<
	cp build/$@ $@

build:
	mkdir -p $@

clean:
	rm -rf build

