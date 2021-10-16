# gnuplot P750-random-2d.dat
set terminal png
#set xrange [-1:1]
#set yrange [-1:1]
set size square
set grid
#set parametric
unset key
#set style line 1 lc rgb "black" lw 3

set output "P750-random-2d-circ.png"
plot "P750-random-2d.dat" \
     using 1:($1**2 + $2**2 <= 1 ? $2 : 1/0) \
     lc rgb "black" lw 1
