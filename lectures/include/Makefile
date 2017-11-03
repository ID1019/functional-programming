LATEXMK = latexmk
PDFLATEX = pdflatex

all : handout-nup.pdf slides.pdf

slides: slides.pdf


handout-nup.pdf: handout.pdf 
	pdfnup --nup 2x2 --landscape  -- handout.pdf

handout.pdf: handout.tex slides.tex 
	$(PDFLATEX) -output-directory=$(@D) handout.tex


%.pdf : %.tex 
	$(LATEXMK) -pdf -pdflatex=$(PDFLATEX) $<


clean:
	$(LATEXMK) -c
	rm -f *.aux *.log *.out *.*~ *~
	rm -f *.nav *.snm *.toc *.vrb

realclean: clean
	rm -f *.pdf
	rm -f *.beam


