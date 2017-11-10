set terminal png
set output "bench.png"

set terminal png linewidth 4

# This is to set the color 
set style line 1 lc rgb "black" lw 1 pt 1

# We scale down
set size 1,1

set title ""

set xlabel "size of tree/table"
set ylabel "time in ms"

#set logscale x
#set xrange [1:3200]
#set yrange [0:200]

plot "bench.dat" u 1:2 with lines title "tree", \
     "" u 1:3 with lines title "tuple"




