# gnuplot P752-lemniscate.plt
set terminal png
set xrange [-1:1]
set yrange [-1:1]
set size square
set grid
set parametric
unset key
set style line 1 lc rgb "black" lw 3

set output "P752-lemniscate.png"
plot cos(t)/(1+sin(t)**2),\
     (sin(t)*(cos(t))/(1+sin(t)**2)) ls 1