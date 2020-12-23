# -*- compile-command: "gnuplot sample0007.gp" -*-
set terminal png
set out "sample0007.tmp.png"
psi(x,y) = cos(x)*sinh(y)
set isosamples 50;
set view 0,0; set noztics # XY 面のみ表示
set nosurface # 等値面を面として表示しない
set contour base # 等値面を XY 面に表示
set cntrparam levels auto 10 #  等高線の本数を増やす
splot [-5:5][0:3] psi(x,y)
