# -*- compile-command: "gnuplot sample0004.gp" -*-
# (1) gnuplot を立ち上げる.
# (2) set xrange[-5:5]  で x の範囲を指定
# (3) set yrange[-5:5]  で y の範囲を指定
# (4) set size ratio 1 で縦横比 1 の正方形の plot にする.
# (5) set palette rgb 33,13,10 でベクトル場の強度を表示するカラーマップをレインボーカラーにする.
# (6) plot "result.dat" u 1:2:3:4:(sqrt ($3*$3+$4*$4)) w vector lc palette ti ""でベクトル場を表示する.
set terminal png
set out "sample0004_tmp.png"
set encoding iso_8859_1
#set term postscript color eps enhanced
#set output "2d-vector.eps"
set size ratio 1
set palette rgb 33,13,10
set xrange[-5:5]
set yrange[-5:5]
set xlabel "x ({\305})"
set ylabel "y ({\305})"
plot sin(x)
replot "sample0004.dat" u 1:2:3:4:(sqrt($3*$3+$4*$4)) w vector lc palette ti ""