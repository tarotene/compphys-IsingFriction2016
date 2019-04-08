set terminal postscript eps color enhanced font ",18"

# range settings
set xrange [25:55]
# set yrange [-0.01:0.07]

# local font settings
set xtics 10 font ",15" nomirror
set ytics 0.0002 font ",15" nomirror
set y2tics 0.0001 font ",15"
set xlabel font ",18"
set ylabel font ",18"
set key font ",18"

# location settings
set xlabel offset screen 0, 0
set ylabel offset screen 0.025, 0
set key at screen 0.98, screen 0.12

# key shape
set key box maxrows 4

# grid settings
# set grid

set output "eps/fric_converge_inset.eps"
set multiplot

# 横寸法
set lmargin screen 0.16
set rmargin screen 0.88

# 縦寸法
tscr=0.96
bscr=0.20
npanel=5
hpanel=(tscr-bscr)/npanel
ipanel=1

# グローバルにx軸を切っておく
unset xlabel
set format x ""

set ylabel "f(4,2.26)"
set yrange [0.0529:0.0535]
set y2range [0.011525:0.011775]
set tmargin screen (tscr-(ipanel-1)*hpanel)
set bmargin screen (tscr-ipanel*hpanel)
plot \
"dat_201803XX/Ising_2d/forinset/Lz004.dat"  u ($1/4):($3/$1) with linespoints notitle ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/forinset/Lz004.dat"  u ($1/4):($9/$1) axis x1y2 with linespoints notitle ps 1 lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/forinset/Lz004.dat"  u ($1/4):($3/$1): ($4/$1) with errorbars title "AP, T=2.26" ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/forinset/Lz004.dat"  u ($1/4):($9/$1):($10/$1) axis x1y2 with errorbars title "P, T=2.26"  ps 1 lw 2 lc rgb '#0000ff'
ipanel=(ipanel+1)

unset key # 2パネル目以降は判例を切る

set ylabel "f(8,2.26)"
set yrange [0.0407:0.0413]
set y2range [0.010325:0.010575]
set tmargin screen (tscr-(ipanel-1)*hpanel)
set bmargin screen (tscr-ipanel*hpanel)
plot \
"dat_201803XX/Ising_2d/forinset/Lz008.dat"  u ($1/8):($3/$1) with linespoints notitle ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/forinset/Lz008.dat"  u ($1/8):($9/$1) axis x1y2 with linespoints notitle ps 1 lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/forinset/Lz008.dat"  u ($1/8):($3/$1): ($4/$1) with errorbars title "AP, T=2.26" ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/forinset/Lz008.dat"  u ($1/8):($9/$1):($10/$1) axis x1y2 with errorbars title "P, T=2.26"  ps 1 lw 2 lc rgb '#0000ff'
ipanel=(ipanel+1)

set ylabel "f(16,2.26)"
set yrange [0.0427:0.0433]
set y2range [0.017225:0.017475]
set tmargin screen (tscr-(ipanel-1)*hpanel)
set bmargin screen (tscr-ipanel*hpanel)
plot \
"dat_201803XX/Ising_2d/forinset/Lz016.dat"  u ($1/16):($3/$1) with linespoints notitle ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/forinset/Lz016.dat"  u ($1/16):($9/$1) axis x1y2 with linespoints notitle ps 1 lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/forinset/Lz016.dat"  u ($1/16):($3/$1): ($4/$1) with errorbars title "AP, T=2.26" ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/forinset/Lz016.dat"  u ($1/16):($9/$1):($10/$1) axis x1y2 with errorbars title "P, T=2.26"  ps 1 lw 2 lc rgb '#0000ff'
ipanel=(ipanel+1)

set ylabel "f(32,2.26)"
set yrange [0.0249:0.0255]
set y2range [0.018125:0.018375]
set tmargin screen (tscr-(ipanel-1)*hpanel)
set bmargin screen (tscr-ipanel*hpanel)
plot \
"dat_201803XX/Ising_2d/forinset/Lz032.dat"  u ($1/32):($3/$1) with linespoints notitle ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/forinset/Lz032.dat"  u ($1/32):($9/$1) axis x1y2 with linespoints notitle ps 1 lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/forinset/Lz032.dat"  u ($1/32):($3/$1): ($4/$1) with errorbars title "AP, T=2.26" ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/forinset/Lz032.dat"  u ($1/32):($9/$1):($10/$1) axis x1y2 with errorbars title "P, T=2.26"  ps 1 lw 2 lc rgb '#0000ff'
ipanel=(ipanel+1)

set format x
set xlabel "L_x / L_z"
set ylabel "f(64,2.26)"
set yrange [0.0203:0.0209]
set y2range [0.018425:0.018675]
set tmargin screen (tscr-(ipanel-1)*hpanel)
set bmargin screen (tscr-ipanel*hpanel)
plot \
"dat_201803XX/Ising_2d/forinset/Lz064.dat"  u ($1/64):($3/$1) with linespoints notitle ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/forinset/Lz064.dat"  u ($1/64):($9/$1) axis x1y2 with linespoints notitle ps 1 lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/forinset/Lz064.dat"  u ($1/64):($3/$1): ($4/$1) with errorbars title "AP, T=2.26" ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/forinset/Lz064.dat"  u ($1/64):($9/$1):($10/$1) axis x1y2 with errorbars title "P, T=2.26"  ps 1 lw 2 lc rgb '#0000ff'
# ipanel=(ipanel+1)

unset multiplot