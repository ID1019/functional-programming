

set key inside left

set xlabel  "n = m+t"

set ylabel  "runtime in ms"

set logscale xy

set xrange [ 120 : 2200 ]
set xtics 32 2

set yrange [ 1 : 300000 ] 
set ytics 2 10 


set style line 1 lw 4

set style line 2 lw 4

p(x) = (x*x)/4000

q(x) = (x*x)/60

r(x) = (x*x*x)/6000

plot "ordered.dat" using 3:4 ls 2 w linespoint title "tree",  \
     "ordered.dat" using 3:5 ls 1 w linespoint title "ordered", \
     p(x) lc 1 title "O(n)", q(x) lc 1 notitle , r(x) lc 3 title "O(n^2)"
