# -*- compile-command: "gnuplot 2020-04-11-233839.tmp.gp" -*-
set terminal png
set datafile separator ","
set ticslevel 0
set xrange [-10:10]
set yrange [-10:10]
set output "2020-04-11-233839.tmp_tmp.png"
plot "workspace/20200411-233802-ode-dynamical-system-001_csv/00000040.csv" u 1:2 pt 7 ps 0.2 lc rgb "#99ccff";
