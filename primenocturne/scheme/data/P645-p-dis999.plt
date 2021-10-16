# gnuplot P645-p-dis999.plt
set terminal png
set output "P645-p-dis999.png"
set xrange [0:1000]
plot 'P645-p-dis999.dat' with lines lt 1 lc rgb "black" lw 3, \
     x/log(x) lt 0 lc rgb "black" lw 5