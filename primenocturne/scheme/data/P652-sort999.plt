# gnuplot P652-sort999.plt
set terminal png
set output "P652-sort999.png"
set xrange [0:999]
set yrange [0:10000]
plot 'P652-sort999.dat' using 1 with points