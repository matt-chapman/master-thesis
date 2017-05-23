all: *.tex Makefile
	cp ~/Documents/Bibliographies/Matt\ Chapman\ Thesis.bib ../bib/library.bib
	pdflatex thesis
	bibtex thesis
	pdflatex -interaction=batchmode thesis
	pdflatex -interaction=batchmode thesis
	mv thesis.pdf ../Thesis.pdf

clean:
	rm -f *~ *.bak *.aux *.log *.toc *.blg *.bbl *.dvi *.ps

bibupdate:
	cp ~/Documents/Bibliographies/Matt\ Chapman\ Thesis.bib ../bib/library.bib