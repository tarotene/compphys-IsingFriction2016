set terminal postscript eps color enhanced font ",20"
set grid

set output "eps/fric_diff.eps"
set yrange [0:0.05]
set ylabel "{/Symbol D}f(L_z,T)"
set xlabel "k_B T/J"
plot \
"dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/result_beta.dat"  u (1/$1):(($2-$8)/200)  w lp title "L_z=4"  lw 2 lc rgb '#800000',\
"dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/result_beta.dat"  u (1/$1):(($2-$8)/400)  w lp title "L_z=8"  lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/result_beta.dat"  u (1/$1):(($2-$8)/800)  w lp title "L_z=16" lw 2 lc rgb '#ff8000',\
"dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/result_beta.dat"  u (1/$1):(($2-$8)/1600) w lp title "L_z=32" lw 2 lc rgb '#ffff00',\
"dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/result_beta.dat"  u (1/$1):(($2-$8)/3200) w lp title "L_z=64" lw 2 lc rgb '#80ff80'  

set output "eps/eb_diff.eps"
set yrange [0.0:1.0]
set ylabel "{/Symbol D}{/Symbol e}(L_z,T)"
set xlabel "k_B T/J"
plot \
"dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/result_beta.dat"  u (1/$1):(($6-$12)/(200*4))   w lp title "L_z=4"  lw 2 lc rgb '#800000',\
"dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/result_beta.dat"  u (1/$1):(($6-$12)/(400*8))   w lp title "L_z=8"  lw 2 lc rgb '#ff0000',\
"dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/result_beta.dat"  u (1/$1):(($6-$12)/(800*16))  w lp title "L_z=16" lw 2 lc rgb '#ff8000',\
"dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/result_beta.dat"  u (1/$1):(($6-$12)/(1600*32)) w lp title "L_z=32" lw 2 lc rgb '#ffff00',\
"dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/result_beta.dat"  u (1/$1):(($6-$12)/(3200*64)) w lp title "L_z=64" lw 2 lc rgb '#80ff80'  