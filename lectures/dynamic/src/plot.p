set terminal png
set output "map.png"

set key inside left

set xlabel  "n = m+t"

set ylabel  "runtime in ms"

set logscale xy

set style line 1 lw 4

set style line 2 lw 4

p(x) = ((x/600)**2)

q(x) = ((x/1400)**4)

plot "map.dat" using 3:4 ls 2 w linespoint title "list",  \
     "map.dat" using 3:5 ls 1 w linespoint title "map", \
     p(x) lc 1 title "O(n^2)", \
     q(x) lc 2 title "O(n^4)

