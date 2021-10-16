# gnuplot P735-sunflower.plt
set terminal png size 1440,960
set size square
set notics
set xrange [-pi:pi]
set yrange [-pi:pi]
unset key
unset border

set output "P735-sunflower.png"
set multiplot layout 3,3

plot "P735-sunflower2.dat" with lines lt 1 lc rgb "black" lw 2
plot "P735-sunflower3.dat" with lines lt 1 lc rgb "black" lw 2
plot "P735-sunflower4.dat" with lines lt 1 lc rgb "black" lw 2
plot "P735-sunflower5.dat" with lines lt 1 lc rgb "black" lw 2
plot "P735-sunflower6.dat" with lines lt 1 lc rgb "black" lw 2
plot "P735-sunflower7.dat" with lines lt 1 lc rgb "black" lw 2
plot "P735-sunflower8.dat" with lines lt 1 lc rgb "black" lw 2
plot "P735-sunflower9.dat" with lines lt 1 lc rgb "black" lw 2
plot "P735-sunflower10.dat" with lines lt 1 lc rgb "black" lw 2

unset multiplot