set terminal postscript eps color enhanced font ",18"

# range settings
set yrange [0.00:0.05]

# local font settings
set xtics 1 font ",15"
set ytics 0.02 font ",15"
set xlabel font ",18"
set ylabel font ",18"
set key font ",18"

# location settings
set xlabel offset screen 0, 0
set ylabel offset screen 0.025, 0
set key at screen 0.96, screen 0.06

# key shape
set key box maxrows 1

# grid settings
set grid

set output "eps/fric_converge_diff.eps"
set multiplot

# 横寸法
set lmargin screen 0.16
set rmargin screen 0.92

# 縦寸法
tscr=0.96
bscr=0.20
npanel=5
hpanel=(tscr-bscr)/npanel
ipanel=1

# グローバルにx軸を切っておく
unset xlabel
set format x ""

set ylabel "{/Symbol D}f(4,T)"
set tmargin screen (tscr-(ipanel-1)*hpanel)
set bmargin screen (tscr-ipanel*hpanel)
plot \
"dat_201803XX/Ising_2d/Lz004Lx0120Ly__Vel10/result_beta.dat"  u (1/$1):(($2-$8)/120) w lp title "AP-P, #1" ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz004Lx0160Ly__Vel10/result_beta.dat"  u (1/$1):(($2-$8)/160) w lp title "AP-P, #2" ps 1 lw 2 lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/result_beta.dat"  u (1/$1):(($2-$8)/200) w lp title "AP-P, #3" ps 1 lw 2 lc rgb '#8b0000'
ipanel=(ipanel+1)

unset key # 2パネル目以降は判例を切る

set ylabel "{/Symbol D}f(8,T)"
set tmargin screen (tscr-(ipanel-1)*hpanel)
set bmargin screen (tscr-ipanel*hpanel)
plot \
"dat_201803XX/Ising_2d/Lz008Lx0240Ly__Vel10/result_beta.dat"  u (1/$1):(($2-$8)/240) w lp title "AP-P, #1" ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz008Lx0320Ly__Vel10/result_beta.dat"  u (1/$1):(($2-$8)/320) w lp title "AP-P, #2" ps 1 lw 2 lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/result_beta.dat"  u (1/$1):(($2-$8)/400) w lp title "AP-P, #3" ps 1 lw 2 lc rgb '#8b0000'
ipanel=(ipanel+1)

set ylabel "{/Symbol D}f(16,T)"
set tmargin screen (tscr-(ipanel-1)*hpanel)
set bmargin screen (tscr-ipanel*hpanel)
plot \
"dat_201803XX/Ising_2d/Lz016Lx0480Ly__Vel10/result_beta.dat"  u (1/$1):(($2-$8)/480) w lp title "AP-P, #1" ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz016Lx0640Ly__Vel10/result_beta.dat"  u (1/$1):(($2-$8)/640) w lp title "AP-P, #2" ps 1 lw 2 lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/result_beta.dat"  u (1/$1):(($2-$8)/800) w lp title "AP-P, #3" ps 1 lw 2 lc rgb '#8b0000'
ipanel=(ipanel+1)

set ylabel "{/Symbol D}f(32,T)"
set tmargin screen (tscr-(ipanel-1)*hpanel)
set bmargin screen (tscr-ipanel*hpanel)
plot \
"dat_201803XX/Ising_2d/Lz032Lx0960Ly__Vel10/result_beta.dat"  u (1/$1):(($2-$8)/960)  w lp title "AP-P, #1"  ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz032Lx1280Ly__Vel10/result_beta.dat"  u (1/$1):(($2-$8)/1280) w lp title "AP-P, #2" ps 1 lw 2 lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/result_beta.dat"  u (1/$1):(($2-$8)/1600) w lp title "AP-P, #3" ps 1 lw 2 lc rgb '#8b0000'
ipanel=(ipanel+1)

set format x
set xlabel "k_B T/J" # 最終パネルにx軸を書く
set ylabel "{/Symbol D}f(64,T)"
set tmargin screen (tscr-(ipanel-1)*hpanel)
set bmargin screen (tscr-ipanel*hpanel)
plot \
"dat_201803XX/Ising_2d/Lz064Lx1920Ly__Vel10/result_beta.dat"  u (1/$1):(($2-$8)/1920) w lp title "AP-P, #1"  ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz064Lx2560Ly__Vel10/result_beta.dat"  u (1/$1):(($2-$8)/2560) w lp title "AP-P, #2" ps 1 lw 2 lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/result_beta.dat"  u (1/$1):(($2-$8)/3200) w lp title "AP-P, #3" ps 1 lw 2 lc rgb '#8b0000'
# ipanel=(ipanel+1)

unset multiplot