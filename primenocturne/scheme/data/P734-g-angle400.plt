# gnuplot P734-g-angle400.plt
set terminal png
set size square
unset key
set output "P734-g-angle400.png"
plot "P734-g-angle400.dat" with point pt 7 ps variable lc rgb "black"
