# gnuplot P740-irr869.plt
set terminal png
set size square
unset tics
unset key
unset border
#set xrange [-10.3:10.3]
#set yrange [-10.3:10.3]

set output "P740-irr869.png"

plot "P740-irr869.dat" with points pt 7 ps variable lc rgb variable
