# gnuplot P664-zero-ratio.plt
set terminal png
set output "P664-zero-ratio.png"
set xrange [0:100]
plot 'P664-zero-ratio2.dat' using 1 with lines, \
     'P664-zero-ratio3.dat' using 1 with lines, \
     'P664-zero-ratio4.dat' using 1 with lines, \
     'P664-zero-ratio5.dat' using 1 with lines
