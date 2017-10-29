set key under
set yrange [-2:2]
set grid

set term postscript eps color
set output fileout.".eps"

plot filein.".dat" using 1:($2/len_x) with lines title "Pumping Density", \
		 filein.".dat" using 1:($3/len_x) with lines title "Dissipation Density", \
		 filein.".dat" using 1:(($2+$3)/len_x) with lines title "Pump.-Diss. Balancing", \
		 filein.".dat" using 1:(($4)/len_x) with lines title "Energy Density"
