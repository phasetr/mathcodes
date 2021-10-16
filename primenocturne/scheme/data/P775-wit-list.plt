# gnuplot P775-wit-list.plt
set terminal png
#set size square
set output "P775-wit-list.png"
set multiplot layout 2,1
plot 'P775-wit-list.dat' using 1 with boxes
plot 'P775-wit-list.dat' using 1 with lines