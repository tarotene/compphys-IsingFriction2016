set key under
set grid

set term postscript eps color

set yrange [-0.1:0.1]
set xlabel "t [MCs]"
set ylabel "P(t), D(t)"
set output fileout1.".eps"
plot filein.".dat" using 1:($2/(len_x)) with lines title "Pumping Density: P(t)", \
		 "" using 1:($3/(len_x)) with lines title "Dissipation Density: D(t)"

set yrange [-2:0]
set xlabel "t [MCs]"
set ylabel "e_b(t)"
set output fileout2.".eps"
plot filein.".dat" using 1:($4/(len_x*len_z)) with lines title "Bulk Energy Density: e_b(t)"
