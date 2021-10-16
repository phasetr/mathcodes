# gnuplot P747-mc-colors.plt
set terminal png
set size square
unset tics
unset key
unset border
set xrange [-10.3:10.3]
set yrange [-10.3:10.3]

set output "P747-mc-colors.png"
plot "P747-mc-colors.dat" \
     with points pt 7 ps variable lc rgb variable