# gnuplot P711-texsq3-rot.plt
set terminal png
unset key
set ticslevel 0
set xtics 1
set ytics 1
set ztics 0.5
set xrange [-5:5]
set yrange [-5:5]
set zrange [-1.5:1.5]
set view 45, 15, 1, 1

set output "P711-texsq3-rot.png"
splot "P711-texsq3-rot.dat" with lines lt 1 lc rgb "black" lw 2
