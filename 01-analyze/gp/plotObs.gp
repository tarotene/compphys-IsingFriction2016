set terminal postscript eps color enhanced
set key maxrows 3 right bottom

set ytics 0.02
set ylabel offset screen 0.01,0

set output "eps/fric_converge.eps"
set multiplot

set yrange [-0.005:0.065]
set lmargin screen 0.10; set rmargin screen 0.95

set tmargin screen 0.95; set bmargin screen 0.78
unset xlabel
set format x ""
set grid
set ylabel "f(4,T)"
plot \
"dat_201803XX/Ising_2d/Lz004Lx0120Ly__Vel10/result_beta.dat"  u (1/$1):($2/120) w lp title "AP, L_x=120" lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz004Lx0160Ly__Vel10/result_beta.dat"  u (1/$1):($2/160) w lp title "AP, L_x=160" lw 2 lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/result_beta.dat"  u (1/$1):($2/200) w lp title "AP, L_x=200" lw 2 lc rgb '#8b0000',\
"dat_201803XX/Ising_2d/Lz004Lx0120Ly__Vel10/result_beta.dat"  u (1/$1):($8/120) w lp title "P, L_x=120"  lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/Lz004Lx0160Ly__Vel10/result_beta.dat"  u (1/$1):($8/160) w lp title "P, L_x=160"  lw 2 lc rgb '#add8e6',\
"dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/result_beta.dat"  u (1/$1):($8/200) w lp title "P, L_x=200"  lw 2 lc rgb '#00008b'  

set tmargin screen 0.78; set bmargin screen 0.61
set ylabel "f(8,T)"
plot \
"dat_201803XX/Ising_2d/Lz008Lx0240Ly__Vel10/result_beta.dat"  u (1/$1):($2/240) w lp title "AP, L_x=240" lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz008Lx0320Ly__Vel10/result_beta.dat"  u (1/$1):($2/320) w lp title "AP, L_x=320" lw 2 lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/result_beta.dat"  u (1/$1):($2/400) w lp title "AP, L_x=400" lw 2 lc rgb '#8b0000',\
"dat_201803XX/Ising_2d/Lz008Lx0240Ly__Vel10/result_beta.dat"  u (1/$1):($8/240) w lp title "P, L_x=240"  lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/Lz008Lx0320Ly__Vel10/result_beta.dat"  u (1/$1):($8/320) w lp title "P, L_x=320"  lw 2 lc rgb '#add8e6',\
"dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/result_beta.dat"  u (1/$1):($8/400) w lp title "P, L_x=400"  lw 2 lc rgb '#00008b'

set tmargin screen 0.61; set bmargin screen 0.44
set ylabel "f(16,T)"
plot \
"dat_201803XX/Ising_2d/Lz016Lx0480Ly__Vel10/result_beta.dat"  u (1/$1):($2/480) w lp title "AP, L_x=480" lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz016Lx0640Ly__Vel10/result_beta.dat"  u (1/$1):($2/640) w lp title "AP, L_x=640" lw 2 lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/result_beta.dat"  u (1/$1):($2/800) w lp title "AP, L_x=800" lw 2 lc rgb '#8b0000',\
"dat_201803XX/Ising_2d/Lz016Lx0480Ly__Vel10/result_beta.dat"  u (1/$1):($8/480) w lp title "P, L_x=480"  lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/Lz016Lx0640Ly__Vel10/result_beta.dat"  u (1/$1):($8/640) w lp title "P, L_x=640"  lw 2 lc rgb '#add8e6',\
"dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/result_beta.dat"  u (1/$1):($8/800) w lp title "P, L_x=800"  lw 2 lc rgb '#00008b'

set tmargin screen 0.44; set bmargin screen 0.27
set ylabel "f(32,T)"
plot \
"dat_201803XX/Ising_2d/Lz032Lx0960Ly__Vel10/result_beta.dat"  u (1/$1):($2/960)  w lp title "AP, L_x=960"  lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz032Lx1280Ly__Vel10/result_beta.dat"  u (1/$1):($2/1280) w lp title "AP, L_x=1280" lw 2 lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/result_beta.dat"  u (1/$1):($2/1600) w lp title "AP, L_x=1600" lw 2 lc rgb '#8b0000',\
"dat_201803XX/Ising_2d/Lz032Lx0960Ly__Vel10/result_beta.dat"  u (1/$1):($8/960)  w lp title "P, L_x=960"   lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/Lz032Lx1280Ly__Vel10/result_beta.dat"  u (1/$1):($8/1280) w lp title "P, L_x=1280"  lw 2 lc rgb '#add8e6',\
"dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/result_beta.dat"  u (1/$1):($8/1600) w lp title "P, L_x=1600"  lw 2 lc rgb '#00008b'

set tmargin screen 0.27; set bmargin screen 0.10
set ylabel "f(64,T)"
set xlabel "k_B T/J"
set format x
plot \
"dat_201803XX/Ising_2d/Lz064Lx1920Ly__Vel10/result_beta.dat"  u (1/$1):($2/1920) w lp title "AP, L_x=960"  lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz064Lx2560Ly__Vel10/result_beta.dat"  u (1/$1):($2/2560) w lp title "AP, L_x=1280" lw 2 lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/result_beta.dat"  u (1/$1):($2/3200) w lp title "AP, L_x=1600" lw 2 lc rgb '#8b0000',\
"dat_201803XX/Ising_2d/Lz064Lx1920Ly__Vel10/result_beta.dat"  u (1/$1):($8/1920) w lp title "P, L_x=960"   lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/Lz064Lx2560Ly__Vel10/result_beta.dat"  u (1/$1):($8/2560) w lp title "P, L_x=1280"  lw 2 lc rgb '#add8e6',\
"dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/result_beta.dat"  u (1/$1):($8/3200) w lp title "P, L_x=1600"  lw 2 lc rgb '#00008b'

unset multiplot

set output "eps/fric_bc.eps"
set multiplot

set key maxrows 2

set yrange [-0.005:0.065]
set lmargin screen 0.10; set rmargin screen 0.95

set tmargin screen 0.95; set bmargin screen 0.55
unset xlabel
set format x ""
set grid
set ylabel "f_{AP}(L_z,T)"
plot \
"dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/result_beta.dat"  u (1/$1):($2/200)  w lp title "L_z=4"  lw 2 lc rgb '#800000',\
"dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/result_beta.dat"  u (1/$1):($2/400)  w lp title "L_z=8"  lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/result_beta.dat"  u (1/$1):($2/800)  w lp title "L_z=16" lw 2 lc rgb '#ff8000',\
"dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/result_beta.dat"  u (1/$1):($2/1600) w lp title "L_z=32" lw 2 lc rgb '#ffff00',\
"dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/result_beta.dat"  u (1/$1):($2/3200) w lp title "L_z=64" lw 2 lc rgb '#80ff80'  

set tmargin screen 0.55; set bmargin screen 0.15
set ylabel "f_P(L_z,T)"
set xlabel "k_B T/J"
set format x
plot \
"dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/result_beta.dat"  u (1/$1):($8/200)  w lp title "L_z=4"  lw 2 lc rgb '#000080',\
"dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/result_beta.dat"  u (1/$1):($8/400)  w lp title "L_z=8"  lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/result_beta.dat"  u (1/$1):($8/800)  w lp title "L_z=16" lw 2 lc rgb '#0080ff',\
"dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/result_beta.dat"  u (1/$1):($8/1600) w lp title "L_z=32" lw 2 lc rgb '#00ffff',\
"dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/result_beta.dat"  u (1/$1):($8/3200) w lp title "L_z=64" lw 2 lc rgb '#80ff80'

unset multiplot

set key maxrows 3

set ytics 1.0
set ylabel offset screen 0,0

set style line 1 lc rgb '#FFF5F0' # very light red
set style line 2 lc rgb '#FEE0D2' # 
set style line 3 lc rgb '#FCBBA1' #
set style line 4 lc rgb '#F7FBFF' # very light blue
set style line 5 lc rgb '#DEEBF7' # 
set style line 6 lc rgb '#C6DBEF' #

set output "eps/eb_converge.eps"
set multiplot

set yrange [-2.25:0.25]
set lmargin screen 0.10; set rmargin screen 0.95

set tmargin screen 0.95; set bmargin screen 0.78
unset xlabel
set format x ""
set grid
set ylabel " {/Symbol e}(4,T)"
plot \
"dat_201803XX/Ising_2d/Lz004Lx0120Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(120*4)) w lp title "AP, L_x=120" lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz004Lx0160Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(160*4)) w lp title "AP, L_x=160" lw 2 lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(200*4)) w lp title "AP, L_x=200" lw 2 lc rgb '#8b0000',\
"dat_201803XX/Ising_2d/Lz004Lx0120Ly__Vel10/result_beta.dat"  u (1/$1):($12/(120*4)) w lp title "P, L_x=120"  lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/Lz004Lx0160Ly__Vel10/result_beta.dat"  u (1/$1):($12/(160*4)) w lp title "P, L_x=160"  lw 2 lc rgb '#add8e6',\
"dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/result_beta.dat"  u (1/$1):($12/(200*4)) w lp title "P, L_x=200"  lw 2 lc rgb '#00008b'

set tmargin screen 0.78; set bmargin screen 0.61
set ylabel " {/Symbol e}(8,T)"
plot \
"dat_201803XX/Ising_2d/Lz008Lx0240Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(240*8)) w lp title "AP, L_x=240" lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz008Lx0320Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(320*8)) w lp title "AP, L_x=320" lw 2 lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(400*8)) w lp title "AP, L_x=400" lw 2 lc rgb '#8b0000',\
"dat_201803XX/Ising_2d/Lz008Lx0240Ly__Vel10/result_beta.dat"  u (1/$1):($12/(240*8)) w lp title "P, L_x=240"  lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/Lz008Lx0320Ly__Vel10/result_beta.dat"  u (1/$1):($12/(320*8)) w lp title "P, L_x=320"  lw 2 lc rgb '#add8e6',\
"dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/result_beta.dat"  u (1/$1):($12/(400*8)) w lp title "P, L_x=400"  lw 2 lc rgb '#00008b'

set tmargin screen 0.61; set bmargin screen 0.44
set ylabel " {/Symbol e}(16,T)"
plot \
"dat_201803XX/Ising_2d/Lz016Lx0480Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(480*16)) w lp title "AP, L_x=480" lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz016Lx0640Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(640*16)) w lp title "AP, L_x=640" lw 2 lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(800*16)) w lp title "AP, L_x=800" lw 2 lc rgb '#8b0000',\
"dat_201803XX/Ising_2d/Lz016Lx0480Ly__Vel10/result_beta.dat"  u (1/$1):($12/(480*16)) w lp title "P, L_x=480"  lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/Lz016Lx0640Ly__Vel10/result_beta.dat"  u (1/$1):($12/(640*16)) w lp title "P, L_x=640"  lw 2 lc rgb '#add8e6',\
"dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/result_beta.dat"  u (1/$1):($12/(800*16)) w lp title "P, L_x=800"  lw 2 lc rgb '#00008b'

set tmargin screen 0.44; set bmargin screen 0.27
set ylabel " {/Symbol e}(32,T)"
plot \
"dat_201803XX/Ising_2d/Lz032Lx0960Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(960 *32)) w lp title "AP, L_x=960"  lw 2   lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz032Lx1280Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(1280*32)) w lp title "AP, L_x=1280" lw 2   lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(1600*32)) w lp title "AP, L_x=1600" lw 2   lc rgb '#8b0000',\
"dat_201803XX/Ising_2d/Lz032Lx0960Ly__Vel10/result_beta.dat"  u (1/$1):($12/(960 *32)) w lp title "P, L_x=960"   lw 2   lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/Lz032Lx1280Ly__Vel10/result_beta.dat"  u (1/$1):($12/(1280*32)) w lp title "P, L_x=1280"  lw 2   lc rgb '#add8e6',\
"dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/result_beta.dat"  u (1/$1):($12/(1600*32)) w lp title "P, L_x=1600"  lw 2   lc rgb '#00008b'

set tmargin screen 0.27; set bmargin screen 0.10
set ylabel " {/Symbol e}(64,T)"
set xlabel "k_B T/J"
set format x
plot \
"dat_201803XX/Ising_2d/Lz064Lx1920Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(1920*64)) w lp title "AP, L_x=960"  lw 2    lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz064Lx2560Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(2560*64)) w lp title "AP, L_x=1280" lw 2    lc rgb '#f03232',\
"dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/result_beta.dat"  u (1/$1):($6 /(3200*64)) w lp title "AP, L_x=1600" lw 2    lc rgb '#8b0000',\
"dat_201803XX/Ising_2d/Lz064Lx1920Ly__Vel10/result_beta.dat"  u (1/$1):($12/(1920*64)) w lp title "P, L_x=960"   lw 2    lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/Lz064Lx2560Ly__Vel10/result_beta.dat"  u (1/$1):($12/(2560*64)) w lp title "P, L_x=1280"  lw 2    lc rgb '#add8e6',\
"dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/result_beta.dat"  u (1/$1):($12/(3200*64)) w lp title "P, L_x=1600"  lw 2    lc rgb '#00008b'

unset multiplot

set output "eps/eb_bc.eps"
set multiplot

set key maxrows 2

set lmargin screen 0.10; set rmargin screen 0.95

set tmargin screen 0.95; set bmargin screen 0.55
unset xlabel
set format x ""
set grid
set ylabel " {/Symbol e}_{AP}(L_z,T)"
plot \
"dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/result_beta.dat"  u (1/$1):($6/(200 * 4)) w lp title "L_z=4"  lw 2 lc rgb '#800000',\
"dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/result_beta.dat"  u (1/$1):($6/(400 * 8)) w lp title "L_z=8"  lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/result_beta.dat"  u (1/$1):($6/(800 *16)) w lp title "L_z=16" lw 2 lc rgb '#ff8000',\
"dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/result_beta.dat"  u (1/$1):($6/(1600*32)) w lp title "L_z=32" lw 2 lc rgb '#ffff00',\
"dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/result_beta.dat"  u (1/$1):($6/(3200*64)) w lp title "L_z=64" lw 2 lc rgb '#80ff80'  

set tmargin screen 0.55; set bmargin screen 0.15
set ylabel " {/Symbol e}_P(L_z,T)"
set xlabel "k_B T/J"
set format x
plot \
"dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/result_beta.dat"  u (1/$1):($12/(200 * 4)) w lp title "L_z=4"  lw 2 lc rgb '#000080',\
"dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/result_beta.dat"  u (1/$1):($12/(400 * 8)) w lp title "L_z=8"  lw 2 lc rgb '#0000ff',\
"dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/result_beta.dat"  u (1/$1):($12/(800 *16)) w lp title "L_z=16" lw 2 lc rgb '#0080ff',\
"dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/result_beta.dat"  u (1/$1):($12/(1600*32)) w lp title "L_z=32" lw 2 lc rgb '#00ffff',\
"dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/result_beta.dat"  u (1/$1):($12/(3200*64)) w lp title "L_z=64" lw 2 lc rgb '#80ff80'

unset multiplot