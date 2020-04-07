set terminal png
set termoption enhanced

set output "fib.png"
set title "fib(40,30), exeution time for 100 runs"


#set logscale y
#set yrange [100:4000]
#set ytics (200,300,400,600,1000,2000,3000,4000)

set yrange [0:4000]


#set logscale x
set xrange [0.8:16]
set xtics (1,2,4,6,8,10,12,14,16)



set xlabel "threads"
set ylabel "time in ms"

set key off
set boxwidth 0.2

#  low, 1q, 3q, high
plot 'fib.dat' using 1:3:2:5:4 with candlesticks
