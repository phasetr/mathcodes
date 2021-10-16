# gnuplot P708-spiral.plt
set terminal png
unset key
set ticslevel 0
set xtics 0.5
set ytics 0.5
set ztics 0.5*3.14
set xlabel "x-axis"
set ylabel "y-axis"
set xrange [-1:1]
set yrange [-1:1]
set zrange [0:2*pi]

set output "P708-spiral-01.png"
set view 60, 30, 1, 1
splot "P708-spiral.dat" with lines lt 1 lc rgb "black" lw 4

set output "P708-spiral-02.png"
set view 90, 0, 1, 0.25*pi
splot "P708-spiral.dat" with lines lt 1 lc rgb "black" lw 4

set output "P708-spiral-03.png"
set view 90, 90, 1, 0.25*pi
splot "P708-spiral.dat" with lines lt 1 lc rgb "black" lw 4