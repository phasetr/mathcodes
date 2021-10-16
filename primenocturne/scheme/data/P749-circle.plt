# gnuplot P749-circle.plt
set terminal png
set xrange [-1:1]
set yrange [-1:1]
set size square
set grid
set parametric
unset key
set style line 1 lc rgb "black" lw 3

set output "P749-circle.png"
plot cos(t),sin(t) ls 1