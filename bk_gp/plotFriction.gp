set terminal postscript eps color enhanced font ",18"

# range settings
set yrange [-0.01:0.07]

# local font settings
set xtics 1 font ",15"
set ytics 0.02 font ",15"
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

set output "eps/fric_converge.eps"
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

set ylabel "f(4,T)"
set tmargin screen (tscr-(ipanel-1)*hpanel)
set bmargin screen (tscr-ipanel*hpanel)
plot \
"dat_201803XX/Ising_2d/Lz004Lx0120Ly__Vel10/result_beta.dat"  u (1/$1):($2/120):($3/120) with errorbars title "AP, #1" ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz004Lx0160Ly__Vel10/result_beta.dat"  u (1/$1):($2/160):($3/160) with errorbars title "AP, #2" ps 1 lw 2 lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/result_beta.dat"  u (1/$1):($2/200):($3/200) with errorbars title "AP, #3" ps 1 lw 2 lc rgb '#8b0000',\
"dat_201803XX/Ising_2d/Lz004Lx0120Ly__Vel10/result_beta.dat"  u (1/$1):($8/120):($9/120) with errorbars title "P, #1"  ps 1 lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/Lz004Lx0160Ly__Vel10/result_beta.dat"  u (1/$1):($8/160):($9/160) with errorbars title "P, #2"  ps 1 lw 2 lc rgb '#add8e6',\
"dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/result_beta.dat"  u (1/$1):($8/200):($9/200) with errorbars title "P, #3"  ps 1 lw 2 lc rgb '#00008b'  
ipanel=(ipanel+1)

unset key # 2パネル目以降は判例を切る

set ylabel "f(8,T)"
set tmargin screen (tscr-(ipanel-1)*hpanel)
set bmargin screen (tscr-ipanel*hpanel)
plot \
"dat_201803XX/Ising_2d/Lz008Lx0240Ly__Vel10/result_beta.dat"  u (1/$1):($2/240):($3/240) with errorbars title "AP, #1" ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz008Lx0320Ly__Vel10/result_beta.dat"  u (1/$1):($2/320):($3/320) with errorbars title "AP, #2" ps 1 lw 2 lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/result_beta.dat"  u (1/$1):($2/400):($3/400) with errorbars title "AP, #3" ps 1 lw 2 lc rgb '#8b0000',\
"dat_201803XX/Ising_2d/Lz008Lx0240Ly__Vel10/result_beta.dat"  u (1/$1):($8/240):($9/240) with errorbars title "P, #1"  ps 1 lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/Lz008Lx0320Ly__Vel10/result_beta.dat"  u (1/$1):($8/320):($9/320) with errorbars title "P, #2"  ps 1 lw 2 lc rgb '#add8e6',\
"dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/result_beta.dat"  u (1/$1):($8/400):($9/400) with errorbars title "P, #3"  ps 1 lw 2 lc rgb '#00008b'
ipanel=(ipanel+1)

set ylabel "f(16,T)"
set tmargin screen (tscr-(ipanel-1)*hpanel)
set bmargin screen (tscr-ipanel*hpanel)
plot \
"dat_201803XX/Ising_2d/Lz016Lx0480Ly__Vel10/result_beta.dat"  u (1/$1):($2/480):($3/480) with errorbars title "AP, #1" ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz016Lx0640Ly__Vel10/result_beta.dat"  u (1/$1):($2/640):($3/640) with errorbars title "AP, #2" ps 1 lw 2 lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/result_beta.dat"  u (1/$1):($2/800):($3/800) with errorbars title "AP, #3" ps 1 lw 2 lc rgb '#8b0000',\
"dat_201803XX/Ising_2d/Lz016Lx0480Ly__Vel10/result_beta.dat"  u (1/$1):($8/480):($9/480) with errorbars title "P, #1"  ps 1 lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/Lz016Lx0640Ly__Vel10/result_beta.dat"  u (1/$1):($8/640):($9/640) with errorbars title "P, #2"  ps 1 lw 2 lc rgb '#add8e6',\
"dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/result_beta.dat"  u (1/$1):($8/800):($9/800) with errorbars title "P, #3"  ps 1 lw 2 lc rgb '#00008b'
ipanel=(ipanel+1)

set ylabel "f(32,T)"
set tmargin screen (tscr-(ipanel-1)*hpanel)
set bmargin screen (tscr-ipanel*hpanel)
plot \
"dat_201803XX/Ising_2d/Lz032Lx0960Ly__Vel10/result_beta.dat"  u (1/$1):($2/ 960):($3/ 960) with errorbars title "AP, #1"  ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz032Lx1280Ly__Vel10/result_beta.dat"  u (1/$1):($2/1280):($3/1280) with errorbars title "AP, #2" ps 1 lw 2 lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/result_beta.dat"  u (1/$1):($2/1600):($3/1600) with errorbars title "AP, #3" ps 1 lw 2 lc rgb '#8b0000',\
"dat_201803XX/Ising_2d/Lz032Lx0960Ly__Vel10/result_beta.dat"  u (1/$1):($8/ 960):($9/ 960) with errorbars title "P, #1"   ps 1 lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/Lz032Lx1280Ly__Vel10/result_beta.dat"  u (1/$1):($8/1280):($9/1280) with errorbars title "P, #2"  ps 1 lw 2 lc rgb '#add8e6',\
"dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/result_beta.dat"  u (1/$1):($8/1600):($9/1600) with errorbars title "P, #3"  ps 1 lw 2 lc rgb '#00008b'
ipanel=(ipanel+1)

set format x
set xlabel "k_B T/J"
set ylabel "f(64,T)"
set tmargin screen (tscr-(ipanel-1)*hpanel)
set bmargin screen (tscr-ipanel*hpanel)
plot \
"dat_201803XX/Ising_2d/Lz064Lx1920Ly__Vel10/result_beta.dat"  u (1/$1):($2/1920):($3/1920) with errorbars title "AP, #1"  ps 1 lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz064Lx2560Ly__Vel10/result_beta.dat"  u (1/$1):($2/2560):($3/2560) with errorbars title "AP, #2" ps 1 lw 2 lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/result_beta.dat"  u (1/$1):($2/3200):($3/3200) with errorbars title "AP, #3" ps 1 lw 2 lc rgb '#8b0000',\
"dat_201803XX/Ising_2d/Lz064Lx1920Ly__Vel10/result_beta.dat"  u (1/$1):($8/1920):($9/1920) with errorbars title "P, #1"   ps 1 lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/Lz064Lx2560Ly__Vel10/result_beta.dat"  u (1/$1):($8/2560):($9/2560) with errorbars title "P, #2"  ps 1 lw 2 lc rgb '#add8e6',\
"dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/result_beta.dat"  u (1/$1):($8/3200):($9/3200) with errorbars title "P, #3"  ps 1 lw 2 lc rgb '#00008b'
# ipanel=(ipanel+1)

unset multiplot