# gnuplot P763-gauss50.plt
set terminal png
set size square
unset key
unset tics
#set xrange [0:1]
#set yrange [0:1]
#set zrange [0:1]
#set view equal xyz
#set grid
#set parametric
#set style line 1 lc rgb "black" lw 3

set output "P763-gauss50.png"
plot "P763-gauss50.dat" every :::0::0 lc rgb "red"   pt 5 ps 0.8,\
     "P763-gauss50.dat" every :::1::1 lc rgb "black" pt 5 ps 0.8