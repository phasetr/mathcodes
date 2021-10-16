# gnuplot P665-bc-ratio-31-32.plt
set terminal png
set output "P665-bc-ratio-31-32.png"
set xrange [0:32]
plot 'P665-bc-ratio-31.dat' using 1 with boxes, \
     'P665-bc-ratio-32.dat' using 1 with lines