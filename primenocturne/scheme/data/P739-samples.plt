# gnuplot P739-repdec.plt
set terminal png size 1440,960
set size square
unset tics
unset key
unset border
#set xrange [-10.3:10.3]
#set yrange [-10.3:10.3]

set output "P739-samples.png"

set multiplot layout 1,3

plot "P739-sample-22-7.dat" with points pt 7 ps variable lc rgb variable
plot "P739-sample-333-106.dat" with points pt 7 ps variable lc rgb variable
plot "P739-sample-355-113.dat" with points pt 7 ps variable lc rgb variable
