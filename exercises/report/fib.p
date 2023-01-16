set terminal pdf
set output 'fib.pdf'

set title ""

set xlabel "n"
set ylabel "time in us"

plot "fib.dat" u 1:2 with lines title "run-time fib(n)"
   
