# gnuplot P671-pascalmono.plt
set terminal png
set size ratio 0.87
set output "P671-pascalmono.png"
plot 'P671-pascalmono.dat' with points pt 7 ps 1 lc rgb variable
