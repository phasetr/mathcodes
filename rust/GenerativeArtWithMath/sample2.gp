# http://gnuplot.sourceforge.net/demo/rectangle.html
set terminal pngcairo  transparent enhanced font "arial,10" fontscale 1.0 size 600, 400
set output 'sample2.tmp.png'
set style fill   pattern 2 border lt 1
set key fixed right top vertical Right noreverse enhanced autotitle box lt black linewidth 1.000 dashtype solid
set key noinvert samplen 4 spacing 1 width 0 height 2
set key opaque
set label 4 "There should be a\nclipped rectangle here" at -2.60000, 0.00000, 0.00000 left norotate back nopoint offset character 0, 0.5, 0
set label 10 "Label in a box" at -3.00000, -4.00000, 0.00000 center norotate front nopoint
set style increment default
set object  1 rect from 0.00000, 0.00000 to 1.00000, 4.00000
set object  1 front clip lw 1.0  dashtype solid fc  rgb "#009e73"  fillstyle   solid 1.00 border lt -1
set object  2 rect from -1.00000, 1.00000 to 0.00000, 5.00000
set object  2 back clip lw 1.0  dashtype solid fc  rgb "gold"  fillstyle   solid 1.00 border lt -1
set object  4 rect center -4.00000, 0.00000 size 3.00000, 1.00000
set object  4 back clip lw 1.0  dashtype solid fc  bgnd fillstyle   solid 1.00 border lt -1
set object  5 rect from 0.00000, -3.00000 to 2.00000, -2.00000
set object  5 back clip lw 1.0  dashtype solid fc  rgb "cyan"  fillstyle   pattern 1 border lt -1
set object  9 rect from -4.00000, -4.00000 to -4.00000, -3.00000
set object  9 back clip lw 1.0  dashtype solid fc  lt -1 fillstyle   solid 1.00 border lt -1
set object 10 rect center -3.00000, -4.00000 size character 14, 1
set object 10 front clip lw 1.0  dashtype solid fc  bgnd fillstyle  empty border lt -1
set object 20 rect from graph 0, 0 to graph 1, 1
set object 20 behind clip lw 1.0  dashtype solid fc  rgb "gold"  fillstyle   solid 0.15 border lt -1
set style data lines
set xrange [ 5.00000 : -5.00000 ] noreverse nowriteback
set x2range [ * : * ] noreverse writeback
set yrange [ * : * ] noreverse writeback
set y2range [ * : * ] noreverse writeback
set zrange [ * : * ] noreverse writeback
set cbrange [ * : * ] noreverse writeback
set rrange [ * : * ] noreverse writeback
LABEL = "Label in a box"
plot x, -3+sin(x*5)/x lt 3 lw 3