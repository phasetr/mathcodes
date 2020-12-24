# -*- compile-command: "gnuplot ch01_sample.gp" -*-
# 参考: http://takeno.iee.niit.ac.jp/~foo/gp-jman/data/20161012/gnuplot-ja-div/node278.html
set terminal png size 800,800
set out "ch01_sample_tmp.png"
set title "ch01-sample"
set size square # 領域を正方形にする
set xrange [0:10] # グラフの範囲指定
set yrange [0:6] # グラフの範囲指定
unset key # 凡例を消す

# 座標軸で囲まれた領域全体の背景を水色に
set object 1 rect from graph 0, graph 0 to graph 1, graph 1 back
set object 1 rect fc rgb "white" fillstyle solid 1.0

# 左下角が 0,0, 右上角が 2,3 の赤い四角を一つ置く
set object 2 rect from 0,0 to 6,6 fc rgb "purple"
set object 3 rect from 6,2 to 10,6 fc rgb "yellow"
set object 4 rect from 6,0 to 8,2 fc rgb "blue"
set object 5 rect from 8,0 to 10,2 fc rgb "red"

# これがないと全体が描画されないので形式的に背景と同じ色の直線を引いている
plot 0 lt rgb "white"
