ELIXIRC=elixirc
GNUPLOT=gnuplot
IEX=iex

.SECONDARY :

all : tree.png table.png comp.png

%.png : %.dat 
	$(GNUPLOT) $*.p

comp.dat : comp.exs comp.p Elixir.bench.beam
	elixir comp.exs  > $@

%.dat : %.exs %.p Elixir.%.beam Elixir.bench.beam
	elixir $*.exs  > $@

Elixir.%.beam : %.ex
	elixirc $*.ex


clean :
	rm -f Elixir.*.beam
	rm -f *.dat

realclean : clean
	rm -f *.png
