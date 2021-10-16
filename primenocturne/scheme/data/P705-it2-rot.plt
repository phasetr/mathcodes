# gnuplot P705-it2-rot.plt
set terminal png
set size square
unset key
unset border

set output "P705-it2-rot.png"
plot 'P705-it2-rot.dat' with lines lt 1 lc rgb "black" lw 2
