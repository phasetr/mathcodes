# gnuplot P747-random-2d.plt
set terminal png
set size square
unset tics
unset key
#unset border
#set xrange [-10.3:10.3]
#set yrange [-10.3:10.3]

set output "P747-random-2d.png"

plot "P747-random-2d.dat"
