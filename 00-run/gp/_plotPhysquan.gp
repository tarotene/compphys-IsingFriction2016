set terminal postscript eps enhanced color
set encoding utf8
set grid

set output file.".eps"

set xrange [0.0:5.0]
set format x ""

set multiplot

set lmargin screen 0.02; set rmargin screen 0.48

set tmargin screen 0.98; set bmargin screen 0.82
set yrange [-2.0:2.0]
plot "physquan1.dat" u (1/$2):4:5 with errorbars title "energy (bulk)"

set tmargin screen 0.82; set bmargin screen 0.66
set yrange [-2.0:2.0]
plot "physquan1.dat" u (1/$2):6:7 with errorbars title "energy (edge)"

set tmargin screen 0.66; set bmargin screen 0.50
set yrange [0:1.0]
plot "physquan1.dat" u (1/$2):8:9 with errorbars title "magnetization (bulk)"

set tmargin screen 0.50; set bmargin screen 0.34
set yrange [0:1.0]
plot "physquan1.dat" u (1/$2):10:11 with errorbars title "magnetization (edge)"

set format x
set tmargin screen 0.34; set bmargin screen 0.18
set yrange [0:1.0]
plot "physquan1.dat" u (1/$2):12:13 with errorbars title "pumping"

set lmargin screen 0.52; set rmargin screen 0.98

set tmargin screen 0.98; set bmargin screen 0.82
set yrange [0:1.0]
plot "physquan2.dat" u (1/$2):4:5 with errorbars title "capacity (bulk)"

set tmargin screen 0.82; set bmargin screen 0.66
set yrange [0:1.0]
plot "physquan2.dat" u (1/$2):6:7 with errorbars title "capacity (edge)"

set tmargin screen 0.66; set bmargin screen 0.50
set yrange [0:1.0]
plot "physquan2.dat" u (1/$2):8:9 with errorbars title "susceptibility (bulk)"

set tmargin screen 0.50; set bmargin screen 0.34
set yrange [0:1.0]
plot "physquan2.dat" u (1/$2):10:11 with errorbars title "susceptibility (edge)"

set tmargin screen 0.34; set bmargin screen 0.18
set yrange [0:1.0]
plot "physquan2.dat" u (1/$2):12:13 with errorbars title "Binder ratio (bulk)"

set format x
set tmargin screen 0.18; set bmargin screen 0.02
set yrange [0:1.0]
plot "physquan2.dat" u (1/$2):14:15 with errorbars title "Binder ratio (edge)"

unset multiplot