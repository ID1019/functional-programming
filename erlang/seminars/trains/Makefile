LATEX =latex
DVIPS =dvips
PS2PDF =ps2pdf

all : assignment.pdf



%.dvi : %.tex
	$(LATEX)  $< 

%.ps : %.dvi
	$(DVIPS) $< 

%.pdf : %.ps
	$(PS2PDF) $< 

clean:
	rm -f *.aux *.log *.out *.*~ *~
	rm -f *.nav *.snm *.toc *.vrb
	rm -f *.dvi *.ps

realclean: clean
	rm -f *.pdf
