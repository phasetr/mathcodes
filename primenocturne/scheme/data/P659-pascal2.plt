# gnuplot P659-pascal2.plt
set terminal png
set output "P659-pascal2.png"
set size ratio 0.87
unset border
unset key
unset tics
set style line 1 pt 7 lc rgb "black"
set style line 2 pt 7 lc rgb "yellow"
plot 'P659-pascal2.dat' using 1:2 every :::0::0 with points ls 1, \
     'P659-pascal2.dat' using 1:2 every :::1::2 with points ls 2