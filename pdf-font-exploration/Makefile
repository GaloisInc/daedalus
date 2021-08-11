
%.all : %.pdf
	pdf2txt.py $<            > $*-p.txt
	pdf-hs-driver --text $<  > $*-g.txt
	mutool draw -F text $<   > $*-m.txt
	pdftotext $<               $*-2.txt

# makes object streams readable:
%-read.pdf : %.pdf
	qpdf -qdf --stream-data=uncompress $< $@

# removes object streams altogether:
%-read2.pdf : %.pdf
	qpdf -qdf --object-streams=disable $< $@




