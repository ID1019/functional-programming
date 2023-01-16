LATEXMK = latexmk
PDFLATEX = pdflatex

all : report.pdf report.tar.gz

report.tar.gz : Makefile report.tex fib.png fib.pdf fib.p fib.dat fib.ex fib.exs
	tar -czvf report.tar.gz  Makefile report.tex fib.png fib.pdf fib.p fib.dat fib.ex fib.exs

fib.dat : fib.ex
	elixirc fib.ex
	elixir fib.exs

fib.pdf : fib.dat
	gnuplot fib.p

fib.png : fib.pdf
	convert fib.pdf fib.png

report.pdf : report.tex fib.png fib.pdf fib.dat screenshot.png 
	$(LATEXMK) -pdf -pdflatex=$(PDFLATEX) -shell-escape $<

clean:
	$(LATEXMK) -c
	rm -f *.aux *.log *.out *.*~ *~
	rm -f *.nav *.snm *.toc *.vrb
	rm -r -f _minted-assignment
	rm -r -f _minted-report
	rm -f src/*.beam
	rm -f *.beam

realclean: clean
	rm -f *.pdf
	rm fib.png
	rm fib.dat
	rm report.tar.gz


