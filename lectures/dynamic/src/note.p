set terminal png
set output "note.png"

set key inside left

set xlabel  "n = m+t"

set ylabel  "runtime in ms"


set style line 1 lw 4

set style line 2 lw 4


plot "note.dat" using 3:4 ls 2 w linespoint title "list",  \
     "note.dat" using 3:5 ls 1 w linespoint title "map"


