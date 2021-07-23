
%.all : %.pdf
	pdf2txt.py $<            > $*-p.txt
	pdf-hs-driver --text $<  > $*-g.txt
	mutool draw -F text $<   > $*-m.txt
	pdftotext $<               $*-2.txt

%-read.pdf : %.pdf
	qpdf -qdf --stream-data=uncompress $< $@




