set terminal postscript eps color enhanced font ",24"

# range settings
set yrange [-2.25:0.25]

# local font settings
set xtics 1 font ",20"
set ytics 0.5 font ",20"
set xlabel font ",24"
set ylabel font ",24"
set key font ",15"

# location settings
set xlabel offset screen 0, 0
set ylabel offset screen 0.025, 0
# set key at screen 0.90, screen 0.10
set key right bottom

# key shape
set key maxrows 2

# grid settings
set grid

set output "eps/eb_bc.eps"
set multiplot

# 横寸法
set lmargin screen 0.16
set rmargin screen 0.96

# 縦寸法
tscr=0.96
bscr=0.16
npanel=2
hpanel=(tscr-bscr)/npanel
ipanel=1

# グローバルにx軸を切っておく
unset xlabel
set format x ""

set ylabel " {/Symbol e}_{AP}(L_z,T)"
set tmargin screen (tscr-(ipanel-1)*hpanel)
set bmargin screen (tscr-ipanel*hpanel)
plot \
"dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/result_beta.dat"  u (1/$1):($6/(200 * 4)) w lp title "L_z=4"  ps 1 lw 2 lc rgb '#800000',\
"dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/result_beta.dat"  u (1/$1):($6/(400 * 8)) w lp title "L_z=8"  ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/result_beta.dat"  u (1/$1):($6/(800 *16)) w lp title "L_z=16" ps 1 lw 2 lc rgb '#ff8000',\
"dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/result_beta.dat"  u (1/$1):($6/(1600*32)) w lp title "L_z=32" ps 1 lw 2 lc rgb '#ffff00',\
"dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/result_beta.dat"  u (1/$1):($6/(3200*64)) w lp title "L_z=64" ps 1 lw 2 lc rgb '#80ff80'  
ipanel=(ipanel+1)

# unset key # 2パネル目以降は判例を切る

set format x
set xlabel "k_B T/J"
set ylabel " {/Symbol e}_P(L_z,T)"
set tmargin screen (tscr-(ipanel-1)*hpanel)
set bmargin screen (tscr-ipanel*hpanel)
plot \
"dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/result_beta.dat"  u (1/$1):($12/(200 * 4)) w lp title "L_z=4"  ps 1 lw 2 lc rgb '#000080',\
"dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/result_beta.dat"  u (1/$1):($12/(400 * 8)) w lp title "L_z=8"  ps 1 lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/result_beta.dat"  u (1/$1):($12/(800 *16)) w lp title "L_z=16" ps 1 lw 2 lc rgb '#0080ff',\
"dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/result_beta.dat"  u (1/$1):($12/(1600*32)) w lp title "L_z=32" ps 1 lw 2 lc rgb '#00ffff',\
"dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/result_beta.dat"  u (1/$1):($12/(3200*64)) w lp title "L_z=64" ps 1 lw 2 lc rgb '#80ff80'
# ipanel=(ipanel+1)

unset multiplot