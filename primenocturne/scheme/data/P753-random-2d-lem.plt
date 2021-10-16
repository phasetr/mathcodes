# gnuplot P753-random-2d-lem.plt
set terminal png
set xrange [0:1]
set yrange [0:1]
set size square
set grid
#set parametric
unset key
#set style line 1 lc rgb "black" lw 3

set output "P753-random-2d-lem.png"
plot "P750-random-2d.dat" \
     using 1:(($1**2 + $2**2)**2 - ($1**2 - $2**2) <= 0 ? $2 : 1/0) \
     lc rgb "black" lw 1
