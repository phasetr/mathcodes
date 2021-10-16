# gnuplot P738-repdec.plt
set terminal png
set size square
unset tics
unset key
unset border
set xrange [-10.3:10.3]
set yrange [-10.3:10.3]

set output "P738-repdec.png"
plot "P738-repdec.dat" \
     with points pt 7 ps variable lc rgb variable