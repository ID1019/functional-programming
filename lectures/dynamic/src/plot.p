set terminal png
set output "map.png"

set key inside left

set xlabel  "n = m+t"

set ylabel  "runtime in ms"

set logscale xy

set style line 1 lw 4

set style line 2 lw 4

p(x) = (x*x)/4000

q(x) = (x*x)/60

r(x) = (x*x*x)/6000

plot "map.dat" using 3:4 ls 2 w linespoint title "list",  \
     "map.dat" using 3:5 ls 1 w linespoint title "map"

