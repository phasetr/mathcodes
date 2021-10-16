# gnuplot P706-texsq2.plt
set terminal png
set size square
unset key
unset border

set output "P706-texsq2.png"
plot 'P706-texsq2.dat' with lines lt 1 lc rgb "black" lw 2
