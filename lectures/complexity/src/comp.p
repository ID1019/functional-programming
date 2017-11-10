set terminal png
set output "comp.png"

set terminal png linewidth 4

# This is to set the color 
set style line 1 lc rgb "black" lw 4 pt 1

# We scale up
set size 1,1

set title ""

set xlabel "number of elements"
set ylabel "time in ms"

set xrange [1:3400]
set yrange [0:200]

plot "comp.dat" u 1:2 with lines title "tree", \
     "" u 1:3 with lines title "tuple"





