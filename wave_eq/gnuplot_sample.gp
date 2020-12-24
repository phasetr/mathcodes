set terminal png
set datafile separator ","
set output "sample_00000000.tmp.png"
set ticslevel 0
set dgrid3d 100,100
splot "workspace/20200403-074318-2dim_wave_eq_u_ut/00000000.csv" u 1:2:3 with lines

set terminal png;set output "sample_00000001.tmp.png";set title "sample_00000001.tmp";set ticslevel 0;set dgrid3d 100,100;splot "workspace/20200403-074318-2dim_wave_eq_u_ut/00000001.csv" u 1:2:3 with lines