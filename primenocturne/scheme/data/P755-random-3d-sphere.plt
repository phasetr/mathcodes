# gnuplot P755-random-3d-sphere.plt
set terminal png
#set xrange [0:1]
#set yrange [0:1]
set zrange [0:1]
set view equal xyz
#set grid
#set parametric
unset key
#set style line 1 lc rgb "black" lw 3

set output "P755-random-3d-sphere.png"

set multiplot layout 1,2
splot "P754-random-3d.dat" using 1:\
      ($1**2 + $2**2 + $3**2 <= 1 ? $2: 1/0):\
      ($1**2 + $2**2 + $3**2 <= 1 ? $3: 1/0) lc rgb "black" lw 1
splot "P754-random-3d.dat" using 1:\
      (($1-0.5)**2 + ($2-0.5)**2 + ($3-0.5)**2 <= 0.5**2 ? $2: 1/0):\
      (($1-0.5)**2 + ($2-0.5)**2 + ($3-0.5)**2 <= 0.5**2 ? $3: 1/0) lc rgb "black" lw 1
