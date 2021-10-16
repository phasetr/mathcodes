# gnuplot P708-it3.plt
set terminal png
set grid
unset key
set xrange [-1:1]
set yrange [-1:1]
set zrange [-1:1]

set output "P708-it3.png"

splot "P708-it3.dat" with lines lt 1 lc rgb "black" lw 6