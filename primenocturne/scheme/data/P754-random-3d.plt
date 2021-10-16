# gnuplot P754-random-3d.plt
set terminal png
set xrange [0:1]
set yrange [0:1]
set size square
set grid
#set parametric
unset key
#set style line 1 lc rgb "black" lw 3

set output "P754-random-3d.png"
splot "P754-random-3d.dat" using 1:2:3 lc rgb "black" lw 1
