# gnuplot P751-asteroid.plt
set terminal png
#set xrange [-1:1]
#set yrange [-1:1]
set size square
set grid
set parametric
unset key
set style line 1 lc rgb "black" lw 3

set output "P751-asteroid.png"
plot cos(t)**3,sin(t)**3 ls 1