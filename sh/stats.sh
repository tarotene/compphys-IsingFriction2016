cat ../../../list_beta.dat | parallel --bar gnuplot -e \"filein=\'beta{}/stream\'\; fileout=\'stats_beta{}\'\" stats.gp
