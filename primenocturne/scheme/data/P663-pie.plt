# gnuplot P663-pie.plt
set terminal png
set output "P663-pie.png"
set size square
unset key
unset border
unset tics
set polar
r=1
plot [0:2*pi] r lc rgb "black" lw 5, \
     'P663-pie.dat' pt 0 lc rgb "black" lw 5 with impulses