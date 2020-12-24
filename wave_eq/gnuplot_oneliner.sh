#!/bin/bash
gnuplot -e 'set terminal png' \
        -e 'set datafile separator ","' \
        -e 'set output "gnuplot_oneliner.tmp.png"'\
        -e 'set ticslevel 0; set dgrid3d 100,100;'\
        -e 'splot "gnuplot_oneliner.csv" u 1:2:3 with lines;'
