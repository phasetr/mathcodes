# gnuplot P752-random-2d-ast.plt
set terminal png
#set xrange [-1:1]
#set yrange [-1:1]
set size square
set grid
#set parametric
unset key
#set style line 1 lc rgb "black" lw 3

set output "P752-random-2d-ast.png"
plot "P750-random-2d.dat" \
     using 1:($1**(2.0/3) + $2**(2.0/3) <= 1 ? $2 : 1/0) \
     lc rgb "black" lw 1
