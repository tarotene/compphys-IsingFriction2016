set terminal postscript eps color enhanced font ",28"

# range settings
set xrange [25:55]
# set yrange [-0.01:0.07]

# local font settings
set xtics 10 nomirror
set ytics 0.0005 nomirror
set y2tics 0.0005
set xlabel font ",32"
set ylabel font ",32"
# set key font ",24"

# location settings
# set xlabel offset screen 0, 0
# set ylabel offset screen 0.025, 0
# set key at screen 0.98, screen 0.12

# key shape
# set key box maxrows 4

# grid settings
# set grid

set output "eps/eb_converge_inset8.eps"
# set multiplot

# 横寸法
# set lmargin screen 0.16
# set rmargin screen 0.88

# 縦寸法
# tscr=0.96
# bscr=0.20
# npanel=5
# hpanel=(tscr-bscr)/npanel
# ipanel=1

# グローバルにx軸を切っておく
# unset xlabel
# set format x ""

set xlabel "L_x/L_z"
set ylabel "{/Symbol e}(8,2.26)"
set yrange [-0.59425:-0.59225]
set y2range [-0.88375:-0.88175]
# set tmargin screen (tscr-(ipanel-1)*hpanel)
# set bmargin screen (tscr-ipanel*hpanel)
plot \
"dat_201803XX/Ising_2d/forinset/Lz008.dat"  u ($1/8):($7/($1*8)) with linespoints notitle ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/forinset/Lz008.dat"  u ($1/8):($13/($1*8)) axis x1y2 with linespoints notitle ps 1 lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/forinset/Lz008.dat"  u ($1/8):($7/($1*8)): ($8/($1*8)) with errorbars title "AP, T=2.26" ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/forinset/Lz008.dat"  u ($1/8):($13/($1*8)):($14/($1*8)) axis x1y2 with errorbars title "P, T=2.26"  ps 1 lw 2 lc rgb '#0000ff'

# unset multiplot