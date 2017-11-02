set terminal png
set output "tuple.png"

set terminal png linewidth 4

# This is to set the color 
set style line 1 lc rgb "black" lw 1 pt 1

# We scale down
set size 1,1

set title ""

set xlabel "size of table in x1000 elements"
set ylabel "time in ms"

#set logscale x
#set xrange [1:40]
#set yrange [0:1400]

plot "tuple.dat" u 1:2 with lines title "insert", \
     "" u 1:3 with lines title "lookup", \
     "" u 1:4 with lines title "modify", \
     "" u 1:5 with lines title "delete"



