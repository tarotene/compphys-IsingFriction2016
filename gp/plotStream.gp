set key under
set yrange [-1:1]
set grid

set term postscript eps color
set output fileout.".eps"

plot filein.".dat" using 1:($2/len_x) smooth unique, \
		 filein.".dat" using 1:($3/len_x) smooth unique, \
		 filein.".dat" using 1:(($2+$3)/len_x) smooth unique
