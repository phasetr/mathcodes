# -*- compile-command: "gnuplot sample0006.gp" -*-
# http://dosei.hatenadiary.jp/entry/20120821/p3
set terminal png
set out "sample0006.tmp.png"
a=0.1
set samples 100
set isosamples 100
set size ratio -1
set xrange [-1:1]
set yrange [-1:1]
plot "++" using 1:2:(-$2*a):($1*a) with vectors
