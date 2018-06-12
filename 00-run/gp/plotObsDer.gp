set terminal postscript eps color enhanced font ",20"
set grid

set output "eps/fric_der.eps"
set multiplot

set yrange [-0.05:0.55]
set lmargin screen 0.20; set rmargin screen 0.95

set tmargin screen 0.95; set bmargin screen 0.55
unset xlabel
set format x ""
set grid
set ylabel "df_{AP}/dT(L_z,T)"
lastx1=0.0; lasty1=0.0; lastx2=0.0; lasty2=0.0; lastx3=0.0; lasty3=0.0; lastx4=0.0; lasty4=0.0; lastx5=0.0; lasty5=0.0
plot "dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/result_beta.dat" u (dy=($2/200 )-lasty1, dx=(1/$1)-lastx1, lasty1=($2/200 ), lastx1=(1/$1), (1/$1)-dx*0.5):($0==0 ? 1/0 : dy/dx) w lp title "L_z=4"  lw 2 lc rgb '#800000',\
     "dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/result_beta.dat" u (dy=($2/400 )-lasty2, dx=(1/$1)-lastx2, lasty2=($2/400 ), lastx2=(1/$1), (1/$1)-dx*0.5):($0==0 ? 1/0 : dy/dx) w lp title "L_z=8"  lw 2 lc rgb '#ff0000',\
     "dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/result_beta.dat" u (dy=($2/800 )-lasty3, dx=(1/$1)-lastx3, lasty3=($2/800 ), lastx3=(1/$1), (1/$1)-dx*0.5):($0==0 ? 1/0 : dy/dx) w lp title "L_z=16" lw 2 lc rgb '#ff8000',\
     "dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/result_beta.dat" u (dy=($2/1600)-lasty4, dx=(1/$1)-lastx4, lasty4=($2/1600), lastx4=(1/$1), (1/$1)-dx*0.5):($0==0 ? 1/0 : dy/dx) w lp title "L_z=32" lw 2 lc rgb '#ffff00',\
     "dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/result_beta.dat" u (dy=($2/3200)-lasty5, dx=(1/$1)-lastx5, lasty5=($2/3200), lastx5=(1/$1), (1/$1)-dx*0.5):($0==0 ? 1/0 : dy/dx) w lp title "L_z=64" lw 2 lc rgb '#80ff80'

set tmargin screen 0.55; set bmargin screen 0.15
set ylabel "df_{P}/dT(L_z,T)"
set xlabel "k_B T/J"
set format x
lastx1=0.0; lasty1=0.0; lastx2=0.0; lasty2=0.0; lastx3=0.0; lasty3=0.0; lastx4=0.0; lasty4=0.0; lastx5=0.0; lasty5=0.0
plot "dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/result_beta.dat" u (dy=($8/200 )-lasty1, dx=(1/$1)-lastx1, lasty1=($8/200 ), lastx1=(1/$1), (1/$1)-dx*0.5):($0==0 ? 1/0 : dy/dx) w lp title "L_z=4"  lw 2 lc rgb '#000080',\
     "dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/result_beta.dat" u (dy=($8/400 )-lasty2, dx=(1/$1)-lastx2, lasty2=($8/400 ), lastx2=(1/$1), (1/$1)-dx*0.5):($0==0 ? 1/0 : dy/dx) w lp title "L_z=8"  lw 2 lc rgb '#0000ff',\
     "dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/result_beta.dat" u (dy=($8/800 )-lasty3, dx=(1/$1)-lastx3, lasty3=($8/800 ), lastx3=(1/$1), (1/$1)-dx*0.5):($0==0 ? 1/0 : dy/dx) w lp title "L_z=16" lw 2 lc rgb '#0080ff',\
     "dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/result_beta.dat" u (dy=($8/1600)-lasty4, dx=(1/$1)-lastx4, lasty4=($8/1600), lastx4=(1/$1), (1/$1)-dx*0.5):($0==0 ? 1/0 : dy/dx) w lp title "L_z=32" lw 2 lc rgb '#00ffff',\
     "dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/result_beta.dat" u (dy=($8/3200)-lasty5, dx=(1/$1)-lastx5, lasty5=($8/3200), lastx5=(1/$1), (1/$1)-dx*0.5):($0==0 ? 1/0 : dy/dx) w lp title "L_z=64" lw 2 lc rgb '#80ff80'

unset multiplot

set output "eps/eb_der.eps"
set multiplot

set yrange [-0.25:2.25]
set lmargin screen 0.20; set rmargin screen 0.95

set tmargin screen 0.95; set bmargin screen 0.55
unset xlabel
set format x ""
set grid
set ylabel "d{/Symbol e}_{AP}/dT(L_z,T)"
lastx1=0.0; lasty1=0.0; lastx2=0.0; lasty2=0.0; lastx3=0.0; lasty3=0.0; lastx4=0.0; lasty4=0.0; lastx5=0.0; lasty5=0.0
plot "dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/result_beta.dat" u (dy=($6/(200*4  ))-lasty1, dx=(1/$1)-lastx1, lasty1=($6/(200*4  )), lastx1=(1/$1), (1/$1)-dx*0.5):($0==0 ? 1/0 : dy/dx) w lp title "L_z=4"  lw 2 lc rgb '#800000',\
     "dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/result_beta.dat" u (dy=($6/(400*8  ))-lasty2, dx=(1/$1)-lastx2, lasty2=($6/(400*8  )), lastx2=(1/$1), (1/$1)-dx*0.5):($0==0 ? 1/0 : dy/dx) w lp title "L_z=8"  lw 2 lc rgb '#ff0000',\
     "dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/result_beta.dat" u (dy=($6/(800*16 ))-lasty3, dx=(1/$1)-lastx3, lasty3=($6/(800*16 )), lastx3=(1/$1), (1/$1)-dx*0.5):($0==0 ? 1/0 : dy/dx) w lp title "L_z=16" lw 2 lc rgb '#ff8000',\
     "dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/result_beta.dat" u (dy=($6/(1600*32))-lasty4, dx=(1/$1)-lastx4, lasty4=($6/(1600*32)), lastx4=(1/$1), (1/$1)-dx*0.5):($0==0 ? 1/0 : dy/dx) w lp title "L_z=32" lw 2 lc rgb '#ffff00',\
     "dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/result_beta.dat" u (dy=($6/(3200*64))-lasty5, dx=(1/$1)-lastx5, lasty5=($6/(3200*64)), lastx5=(1/$1), (1/$1)-dx*0.5):($0==0 ? 1/0 : dy/dx) w lp title "L_z=64" lw 2 lc rgb '#80ff80'

set tmargin screen 0.55; set bmargin screen 0.15
set ylabel "d{/Symbol e}_{P}/dT(L_z,T)"
set xlabel "k_B T/J"
set format x
lastx1=0.0; lasty1=0.0; lastx2=0.0; lasty2=0.0; lastx3=0.0; lasty3=0.0; lastx4=0.0; lasty4=0.0; lastx5=0.0; lasty5=0.0
plot "dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/result_beta.dat" u (dy=($12/(200*4  ))-lasty1, dx=(1/$1)-lastx1, lasty1=($12/(200*4  )), lastx1=(1/$1), (1/$1)-dx*0.5):($0==0 ? 1/0 : dy/dx) w lp title "L_z=4"  lw 2 lc rgb '#000080',\
     "dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/result_beta.dat" u (dy=($12/(400*8  ))-lasty2, dx=(1/$1)-lastx2, lasty2=($12/(400*8  )), lastx2=(1/$1), (1/$1)-dx*0.5):($0==0 ? 1/0 : dy/dx) w lp title "L_z=8"  lw 2 lc rgb '#0000ff',\
     "dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/result_beta.dat" u (dy=($12/(800*16 ))-lasty3, dx=(1/$1)-lastx3, lasty3=($12/(800*16 )), lastx3=(1/$1), (1/$1)-dx*0.5):($0==0 ? 1/0 : dy/dx) w lp title "L_z=16" lw 2 lc rgb '#0080ff',\
     "dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/result_beta.dat" u (dy=($12/(1600*32))-lasty4, dx=(1/$1)-lastx4, lasty4=($12/(1600*32)), lastx4=(1/$1), (1/$1)-dx*0.5):($0==0 ? 1/0 : dy/dx) w lp title "L_z=32" lw 2 lc rgb '#00ffff',\
     "dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/result_beta.dat" u (dy=($12/(3200*64))-lasty5, dx=(1/$1)-lastx5, lasty5=($12/(3200*64)), lastx5=(1/$1), (1/$1)-dx*0.5):($0==0 ? 1/0 : dy/dx) w lp title "L_z=64" lw 2 lc rgb '#80ff80'

unset multiplot