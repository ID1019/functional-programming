LATEXMK = latexmk
PDFLATEX = pdflatex


all : assignment.pdf


%.pdf : %.tex 
	$(LATEXMK) -pdf -pdflatex=$(PDFLATEX) $<

clean:
	$(LATEXMK) -c
	rm -f *.aux *.log *.out *.*~ *~
	rm -f *.nav *.snm *.toc *.vrb
	rm -f src/*.beam
	rm -f *.beam

realclean: clean
	rm -f *.pdf



