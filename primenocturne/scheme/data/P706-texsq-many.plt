# gnuplot P706-texsq-many.plt
set terminal png size 1440,960
set size square
unset key
unset border

set output "P706-texsq-many.png"

set multiplot layout 6,4

plot 'P706-texsq-many-1.dat' with lines lt 1 lc rgb "black" lw 2
plot 'P706-texsq-many-2.dat' with lines lt 1 lc rgb "black" lw 2
plot 'P706-texsq-many-3.dat' with lines lt 1 lc rgb "black" lw 2
plot 'P706-texsq-many-4.dat' with lines lt 1 lc rgb "black" lw 2
plot 'P706-texsq-many-5.dat' with lines lt 1 lc rgb "black" lw 2
plot 'P706-texsq-many-6.dat' with lines lt 1 lc rgb "black" lw 2
plot 'P706-texsq-many-7.dat' with lines lt 1 lc rgb "black" lw 2
plot 'P706-texsq-many-8.dat' with lines lt 1 lc rgb "black" lw 2
plot 'P706-texsq-many-9.dat' with lines lt 1 lc rgb "black" lw 2
plot 'P706-texsq-many-10.dat' with lines lt 1 lc rgb "black" lw 2
plot 'P706-texsq-many-11.dat' with lines lt 1 lc rgb "black" lw 2
plot 'P706-texsq-many-12.dat' with lines lt 1 lc rgb "black" lw 2
plot 'P706-texsq-many-13.dat' with lines lt 1 lc rgb "black" lw 2
plot 'P706-texsq-many-14.dat' with lines lt 1 lc rgb "black" lw 2
plot 'P706-texsq-many-15.dat' with lines lt 1 lc rgb "black" lw 2
plot 'P706-texsq-many-16.dat' with lines lt 1 lc rgb "black" lw 2
plot 'P706-texsq-many-17.dat' with lines lt 1 lc rgb "black" lw 2
plot 'P706-texsq-many-18.dat' with lines lt 1 lc rgb "black" lw 2
plot 'P706-texsq-many-19.dat' with lines lt 1 lc rgb "black" lw 2
plot 'P706-texsq-many-20.dat' with lines lt 1 lc rgb "black" lw 2
plot 'P706-texsq-many-21.dat' with lines lt 1 lc rgb "black" lw 2
plot 'P706-texsq-many-22.dat' with lines lt 1 lc rgb "black" lw 2
plot 'P706-texsq-many-23.dat' with lines lt 1 lc rgb "black" lw 2
plot 'P706-texsq-many-24.dat' with lines lt 1 lc rgb "black" lw 2

unset multiplot