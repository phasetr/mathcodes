# -*- compile-command: "gnuplot sample0003.gp" -*-
# http://dosei.hatenadiary.jp/entry/20120821/p3
set terminal png
set out "sample0003_ex1.tmp.png"
plot x*x

set out "sample0003_ex2.tmp.png"
plot "+" using 1:($1*$1) with lines

set out "sample0003_ex3.tmp.png"
a=0.1
set samples 200
set isosamples 200
set size ratio -1
set xrange [-1:1]
set yrange [-1:1]
plot "++" using 1:2:(-$2*a):($1*a) with vectors