# https://qiita.com/xr0038/items/80e30db5fa7bcd391b18
set terminal png
set output "sample0001.tmp.png"
set xrange [0:2*pi]
set sample 500
set key outside
plot for [n=1:7] sin(n*x/2.) t sprintf("sin(%dx/2)",n) lw 2