# -*- compile-command: "gnuplot sample1.gp" -*-
set terminal png
set out "sample1.tmp.png"
set size square
set parametric
plot [0:2*pi] cos(t),sin(t)
