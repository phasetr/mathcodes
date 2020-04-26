# -*- compile-command: "gnuplot ode_dynamical_system001_visualize.gp" -*-
set terminal png
set print "-"
set datafile separator ","
set output "ode_dynamical_system001_visualize_tmp.png"
set ticslevel 0
set xrange [-10:10]
set yrange [-10:10]
print "TEST"
plot "workspace/20200412-072212-ode-dynamical-system-001_csv/00004999.csv" u 1:2 pt 7 ps 0.1 lc rgb '#99ccff'
#print "ode_dynamical_system001_visualize_tmp.png"
