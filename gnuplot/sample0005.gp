# -*- compile-command: "gnuplot sample0005.gp" -*-
# https://meltingrabbit.com/blog/article/2017101103/
# グラフの重なり順設定の注意
# gnuplot では後から書いたグラフが上層へくるので表面に出したいものを後でプロットする．

set y2tics
# 多数のベクトルで塗りつぶしを表現, gnuplot内でデータを演算し，ベクトルのスケールを変更
plot "sample0005_FlowVectorAll.dat"\
     u 1:2:($3*0.002):($4*0.002)\
     w vector empty nohead lc rgb "pink" lw 2 ti ""
# 線の太さを設定
replot "sample0005_Cf.dat"\
       u 1:2\
       w l lc rgb "black" lw 2 ti "{/Consolas:Italic C}_{/Consolas:Italic f}" axes x1y2
# 線の太さを設定
replot "sample0005_Bt.dat"\
       u 1:2\
       w l lc rgb "blue"  lw 2 ti "Boundary Layer Thickness"
# gnuplot内でデータを演算し，ベクトルのスケールを変更
# every で何とかしたい: 次のようなコード
#replot for [i=0:10] "FlowVectorAll.dat" \
#       every ::::25\
#       u 1:2:($3*0.002):($4*0.002)  w vector lw 2 lc rgb "red" ti "Flow Velocity"
replot "sample0005_FlowVector.dat"\
       u 1:2:($3*0.002):($4*0.002)\
       w vector lw 2 lc rgb "red" ti "Flow Velocity"

# 凡例背景の編集
set key outside opaque box linewidth 15 linecolor rgb "white" height 0.2 width 0 invert

set xlabel  "{/Consolas:Italic x} [m]"
set ylabel  "thickness [m]"
set y2label "{/Consolas:Italic C}_{/Consolas:Italic f} []" rotate by -90
set xrange [0:0.55]
set yrange [0:0.007]
set y2range[0:0.025]
set format x  "%.1f"
set format y  "%.3f"
set format y2 "%.3f"
replot

# フォント・フォントサイズを変更
set tics    font "Consolas,12"
set key     font "Consolas,12"
set title   font "Consolas,12"
set xlabel  font "Consolas,12"
set ylabel  font "Consolas,12"
set zlabel  font "Consolas,12"
set y2label font "Consolas,12"
set cblabel font "Consolas,12"

#set terminal pngcairo enhanced color size 6.0in, 4.5in
#set output "sample0005.tmp.png"
# 出力画像のサイズを指定
set terminal pdf enhanced color size 10.0in, 4.5in
set output "sample0005.tmp.pdf"
replot
