# gnuplot P703-polygon.plt
set terminal png
set size square
unset key
unset border
set xrange [-1:1]
set yrange [-1:1]

set output "P703-polygon3.png"
plot 'P703-polygon3.dat' with lines lt 1 lc rgb "black" lw 2
set output "P703-polygon4.png"
plot 'P703-polygon4.dat' with lines lt 1 lc rgb "black" lw 2
set output "P703-polygon5.png"
plot 'P703-polygon5.dat' with lines lt 1 lc rgb "black" lw 2
set output "P703-polygon6.png"
plot 'P703-polygon6.dat' with lines lt 1 lc rgb "black" lw 2
