set terminal pngcairo  enhanced font "arial,10" fontscale 1.0 size 800, 600 


set title "Execution time fb(40,30), 100 iterations, time in ms" 
set xrange[0:26]
set yrange[0:8000]

set bars 4

# Data columns: X Min 1stQuartile Median 3rdQuartile Max 

set xtics 1
set ytics 1000
set output 'first.png'
plot 'bench.dat' using 1:($3/1000):($2/1000):($6/1000):($5/1000):xticlabels(8) with candlesticks title 'Quartiles' lw 1

set yrange[400:8000]
set ytics (400,500,600,700,800,1000,2000,3000,4000,5000,6000,7000,8000)
set logscale y

set output 'logy.png'

plot 'bench.dat' using 1:($3/1000):($2/1000):($6/1000):($5/1000):xticlabels(8) with candlesticks title 'Quartiles' lw 1

   set xrange[0.8:26]
set logscale xy
set xtics (1,2,3,4,5,6,8,10,12,16,20,24)
set output 'logxy.png'

plot 'bench.dat' using 1:($3/1000):($2/1000):($6/1000):($5/1000):xticlabels(8) with candlesticks title 'Quartiles' lw 1


