set terminal png
set output "nth.png"

# This is to set the color 
set style line 1 lc rgb "black" lw 1 pt 1

# We scale down
set size 1,1

set title ""

set xlabel "n"
set ylabel "time in ms"

plot "nth.dat" u 1:2 with lines title "dummy", \
     "" u 1:3 with lines title "recursive list", \
     "" u 1:4 with lines title "explict list", \
     "" u 1:5 with lines title "builtin element",\
     ""  u 1:6 with lines title "explicit tuple"


