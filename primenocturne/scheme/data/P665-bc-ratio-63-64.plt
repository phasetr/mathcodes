# gnuplot P665-bc-ratio-63-64.plt
set terminal png
set output "P665-bc-ratio-63-64.png"
set xrange [0:64]
set yrange [0:0.1]
plot 'P665-bc-ratio-63.dat' using 1 with boxes, \
     'P665-bc-ratio-64.dat' using 1 with lines