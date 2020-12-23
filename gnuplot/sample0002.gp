# -*- compile-command: "gnuplot sample0002.gp" -*-
# https://qiita.com/xr0038/items/80e30db5fa7bcd391b18
load 'color.gp'
set terminal png
set out "sample0002_tmp.png"
set xrange [0:2*pi]
set sample 500
set key outside
plot for [n=1:7] sin(n*x/2.) t sprintf("sin(%dx/2)",n) lw 2 \
     lc rgb hsv(180+15*n,200-20*n,255)
