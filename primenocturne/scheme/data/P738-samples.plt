# gnuplot P738-repdec.plt
set terminal png size 1440,960
set size square
unset tics
unset key
unset border
# set xrange [-10.3:10.3]
# set yrange [-10.3:10.3]

set output "P738-samples.png"

set multiplot layout 2,3

plot "P738-sample-1-7.dat" with points pt 7 ps variable lc rgb variable
plot "P738-sample-1-11.dat" with points pt 7 ps variable lc rgb variable
plot "P738-sample-1-13.dat" with points pt 7 ps variable lc rgb variable
plot "P738-sample-1-17.dat" with points pt 7 ps variable lc rgb variable
plot "P738-sample-1-97.dat" with points pt 7 ps variable lc rgb variable
plot "P738-sample-1-2539.dat" with points pt 7 ps variable lc rgb variable
