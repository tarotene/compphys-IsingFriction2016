set terminal postscript eps color enhanced font ",20"
set grid

T21=1.2504; T22=1.8515; T23=2.2523; T24=2.3725; T25=2.3924
T31=1.7512; T32=1.9607; T33=2.2523; T34=2.3725; T35=2.3924
T41=2.5521; T42=2.4907; T43=2.4540; T44=2.4331; T45=2.4126
T51=2.6534; T52=2.4907; T53=2.4722; T54=2.3924; T55=2.3311
L01=     4; L02=     8; L03=    16; L04=    32; L05=    64

set output "eps/fric_dersc.eps"
set multiplot

nu=1.0
a=-0.5

# set yrange [-1:17]
# set ytics 4
set xrange [-20:20]
set lmargin screen 0.20; set rmargin screen 0.95

set tmargin screen 0.95; set bmargin screen 0.55
unset xlabel
set format x ""
set grid
set ylabel "df_{AP}/dT(L_z,T)"
plot "dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/d_result_beta.dat" u (L01*sgn($1-T21)*abs($1-T21)**nu):((L01**a)*$2) w lp title "L_z=4"  lw 2 lc rgb '#800000',\
     "dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/d_result_beta.dat" u (L02*sgn($1-T22)*abs($1-T22)**nu):((L02**a)*$2) w lp title "L_z=8"  lw 2 lc rgb '#ff0000',\
     "dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/d_result_beta.dat" u (L03*sgn($1-T23)*abs($1-T23)**nu):((L03**a)*$2) w lp title "L_z=16" lw 2 lc rgb '#ff8000',\
     "dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/d_result_beta.dat" u (L04*sgn($1-T24)*abs($1-T24)**nu):((L04**a)*$2) w lp title "L_z=32" lw 2 lc rgb '#ffff00',\
     "dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/d_result_beta.dat" u (L05*sgn($1-T25)*abs($1-T25)**nu):((L05**a)*$2) w lp title "L_z=64" lw 2 lc rgb '#80ff80'

set tmargin screen 0.55; set bmargin screen 0.15
set ylabel "df_{P}/dT(L_z,T)"
set xlabel "k_B T/J"
set format x
plot "dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/d_result_beta.dat" u (L01*sgn($1-T41)*abs($1-T41)**nu):((L01**a)*$4) w lp title "L_z=4"  lw 2 lc rgb '#000080',\
     "dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/d_result_beta.dat" u (L02*sgn($1-T42)*abs($1-T42)**nu):((L02**a)*$4) w lp title "L_z=8"  lw 2 lc rgb '#0000ff',\
     "dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/d_result_beta.dat" u (L03*sgn($1-T43)*abs($1-T43)**nu):((L03**a)*$4) w lp title "L_z=16" lw 2 lc rgb '#0080ff',\
     "dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/d_result_beta.dat" u (L04*sgn($1-T44)*abs($1-T44)**nu):((L04**a)*$4) w lp title "L_z=32" lw 2 lc rgb '#00ffff',\
     "dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/d_result_beta.dat" u (L05*sgn($1-T45)*abs($1-T45)**nu):((L05**a)*$4) w lp title "L_z=64" lw 2 lc rgb '#80ff80'

unset multiplot

set output "eps/eb_dersc.eps"
set multiplot

nu=1.0
a=-0.5

# set yrange [-0.1:2.1]
# set ytics 0.5
set xrange [-50:50]
set lmargin screen 0.20; set rmargin screen 0.95

set tmargin screen 0.95; set bmargin screen 0.55
unset xlabel
set format x ""
set grid
set ylabel "d{/Symbol e}_{AP}/dT(L_z,T)"
plot "dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/d_result_beta.dat" u (L01*sgn($1-T31)*abs($1-T31)**nu):((L01**a)*$3) w lp title "L_z=4"  lw 2 lc rgb '#800000',\
     "dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/d_result_beta.dat" u (L02*sgn($1-T32)*abs($1-T32)**nu):((L02**a)*$3) w lp title "L_z=8"  lw 2 lc rgb '#ff0000',\
     "dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/d_result_beta.dat" u (L03*sgn($1-T33)*abs($1-T33)**nu):((L03**a)*$3) w lp title "L_z=16" lw 2 lc rgb '#ff8000',\
     "dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/d_result_beta.dat" u (L04*sgn($1-T34)*abs($1-T34)**nu):((L04**a)*$3) w lp title "L_z=32" lw 2 lc rgb '#ffff00',\
     "dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/d_result_beta.dat" u (L05*sgn($1-T35)*abs($1-T35)**nu):((L05**a)*$3) w lp title "L_z=64" lw 2 lc rgb '#80ff80'

set tmargin screen 0.55; set bmargin screen 0.15
set ylabel "d{/Symbol e}_{P}/dT(L_z,T)"
set xlabel "k_B T/J"
set format x
plot "dat_201803XX/Ising_2d/Lz004Lx0200Ly__Vel10/d_result_beta.dat" u (L01*sgn($1-T51)*abs($1-T51)**nu):((L01**a)*$5) w lp title "L_z=4"  lw 2 lc rgb '#000080',\
     "dat_201803XX/Ising_2d/Lz008Lx0400Ly__Vel10/d_result_beta.dat" u (L02*sgn($1-T52)*abs($1-T52)**nu):((L02**a)*$5) w lp title "L_z=8"  lw 2 lc rgb '#0000ff',\
     "dat_201803XX/Ising_2d/Lz016Lx0800Ly__Vel10/d_result_beta.dat" u (L03*sgn($1-T53)*abs($1-T53)**nu):((L03**a)*$5) w lp title "L_z=16" lw 2 lc rgb '#0080ff',\
     "dat_201803XX/Ising_2d/Lz032Lx1600Ly__Vel10/d_result_beta.dat" u (L04*sgn($1-T54)*abs($1-T54)**nu):((L04**a)*$5) w lp title "L_z=32" lw 2 lc rgb '#00ffff',\
     "dat_201803XX/Ising_2d/Lz064Lx3200Ly__Vel10/d_result_beta.dat" u (L05*sgn($1-T55)*abs($1-T55)**nu):((L05**a)*$5) w lp title "L_z=64" lw 2 lc rgb '#80ff80'

unset multiplot