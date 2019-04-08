set terminal postscript eps color enhanced font ",18"

# range settings
set yrange [-2.25:0.25]

# local font settings
set xtics 1 font ",15"
set ytics 0.5 font ",15"
set xlabel font ",18"
set ylabel font ",18"
set key font ",18"

# location settings
set xlabel offset screen 0, 0
set ylabel offset screen 0.025, 0
set key at screen 0.98, screen 0.12

# key shape
set key box maxrows 3

# grid settings
set grid

set output "eps/eb_converge.eps"
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

set ylabel " {/Symbol e}(4,T)"
set tmargin screen (tscr-(ipanel-1)*hpanel)
set bmargin screen (tscr-ipanel*hpanel)
plot \
"dat_201803XX/Ising_2d/Lz004Lx0120Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(120*4)):($7 /(120*4)) with errorbars title "AP, #1" ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz004Lx0160Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(160*4)):($7 /(160*4)) with errorbars title "AP, #2" ps 1 lw 2 lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(200*4)):($7 /(200*4)) with errorbars title "AP, #3" ps 1 lw 2 lc rgb '#8b0000',\
"dat_201803XX/Ising_2d/Lz004Lx0120Ly__Vel10/result_beta.dat"  u (1/$1):($12/(120*4)):($13/(120*4)) with errorbars title "P, #1"  ps 1 lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/Lz004Lx0160Ly__Vel10/result_beta.dat"  u (1/$1):($12/(160*4)):($13/(160*4)) with errorbars title "P, #2"  ps 1 lw 2 lc rgb '#add8e6',\
"dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/result_beta.dat"  u (1/$1):($12/(200*4)):($13/(200*4)) with errorbars title "P, #3"  ps 1 lw 2 lc rgb '#00008b'
ipanel=(ipanel+1)

unset key # 2パネル目以降は判例を切る

set ylabel " {/Symbol e}(8,T)"
set tmargin screen (tscr-(ipanel-1)*hpanel)
set bmargin screen (tscr-ipanel*hpanel)
plot \
"dat_201803XX/Ising_2d/Lz008Lx0240Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(240*8)):($7 /(240*8)) with errorbars title "AP, #1" ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz008Lx0320Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(320*8)):($7 /(320*8)) with errorbars title "AP, #2" ps 1 lw 2 lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(400*8)):($7 /(400*8)) with errorbars title "AP, #3" ps 1 lw 2 lc rgb '#8b0000',\
"dat_201803XX/Ising_2d/Lz008Lx0240Ly__Vel10/result_beta.dat"  u (1/$1):($12/(240*8)):($13/(240*8)) with errorbars title "P, #1"  ps 1 lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/Lz008Lx0320Ly__Vel10/result_beta.dat"  u (1/$1):($12/(320*8)):($13/(320*8)) with errorbars title "P, #2"  ps 1 lw 2 lc rgb '#add8e6',\
"dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/result_beta.dat"  u (1/$1):($12/(400*8)):($13/(400*8)) with errorbars title "P, #3"  ps 1 lw 2 lc rgb '#00008b'
ipanel=(ipanel+1)

set tmargin screen (tscr-(ipanel-1)*hpanel)
set bmargin screen (tscr-ipanel*hpanel)
set ylabel " {/Symbol e}(16,T)"
plot \
"dat_201803XX/Ising_2d/Lz016Lx0480Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(480*16)):($7 /(480*16)) with errorbars title "AP, #1" ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz016Lx0640Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(640*16)):($7 /(640*16)) with errorbars title "AP, #2" ps 1 lw 2 lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(800*16)):($7 /(800*16)) with errorbars title "AP, #3" ps 1 lw 2 lc rgb '#8b0000',\
"dat_201803XX/Ising_2d/Lz016Lx0480Ly__Vel10/result_beta.dat"  u (1/$1):($12/(480*16)):($13/(480*16)) with errorbars title "P, #1"  ps 1 lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/Lz016Lx0640Ly__Vel10/result_beta.dat"  u (1/$1):($12/(640*16)):($13/(640*16)) with errorbars title "P, #2"  ps 1 lw 2 lc rgb '#add8e6',\
"dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/result_beta.dat"  u (1/$1):($12/(800*16)):($13/(800*16)) with errorbars title "P, #3"  ps 1 lw 2 lc rgb '#00008b'
ipanel=(ipanel+1)

set ylabel " {/Symbol e}(32,T)"
set tmargin screen (tscr-(ipanel-1)*hpanel)
set bmargin screen (tscr-ipanel*hpanel)
plot \
"dat_201803XX/Ising_2d/Lz032Lx0960Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(960 *32)):($7 /(960 *32)) with errorbars title "AP, #1"  ps 1 lw 2   lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz032Lx1280Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(1280*32)):($7 /(1280*32)) with errorbars title "AP, #2" ps 1 lw 2   lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(1600*32)):($7 /(1600*32)) with errorbars title "AP, #20" ps 1 lw 2   lc rgb '#8b0000',\
"dat_201803XX/Ising_2d/Lz032Lx0960Ly__Vel10/result_beta.dat"  u (1/$1):($12/(960 *32)):($13/(960 *32)) with errorbars title "P, #1"   ps 1 lw 2   lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/Lz032Lx1280Ly__Vel10/result_beta.dat"  u (1/$1):($12/(1280*32)):($13/(1280*32)) with errorbars title "P, #2"  ps 1 lw 2   lc rgb '#add8e6',\
"dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/result_beta.dat"  u (1/$1):($12/(1600*32)):($13/(1600*32)) with errorbars title "P, #20"  ps 1 lw 2   lc rgb '#00008b'
ipanel=(ipanel+1)

set format x
set xlabel "k_B T/J" # 最終パネルにx軸を書く
set ylabel " {/Symbol e}(64,T)"
set tmargin screen (tscr-(ipanel-1)*hpanel)
set bmargin screen (tscr-ipanel*hpanel)
plot \
"dat_201803XX/Ising_2d/Lz064Lx1920Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(1920*64)):($7 /(1920*64)) with errorbars title "AP, #1"  ps 1 lw 2    lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz064Lx2560Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(2560*64)):($7 /(2560*64)) with errorbars title "AP, #2" ps 1 lw 2    lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(3200*64)):($7 /(3200*64)) with errorbars title "AP, #20" ps 1 lw 2    lc rgb '#8b0000',\
"dat_201803XX/Ising_2d/Lz064Lx1920Ly__Vel10/result_beta.dat"  u (1/$1):($12/(1920*64)):($13/(1920*64)) with errorbars title "P, #1"   ps 1 lw 2    lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/Lz064Lx2560Ly__Vel10/result_beta.dat"  u (1/$1):($12/(2560*64)):($13/(2560*64)) with errorbars title "P, #2"  ps 1 lw 2    lc rgb '#add8e6',\
"dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/result_beta.dat"  u (1/$1):($12/(3200*64)):($13/(3200*64)) with errorbars title "P, #20"  ps 1 lw 2    lc rgb '#00008b'
# ipanel=(ipanel+1)

unset multiplot