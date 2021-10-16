# gnuplot P705-it2.plt
set terminal png
set size square
unset key
unset border

set output "P705-it2.png"
plot 'P705-it2.dat' with lines lt 1 lc rgb "black" lw 2
