se te po eps enh col
se gr
se k r b

# set tics font ",24"   # 目盛りのフォントの変更
# set xlabel font ",32" # xlabelのフォントの変更
# set ylabel font ",32" # ylabelのフォントの変更
# set zlabel font ",32" # zlabelのフォントの変更
# set key font ",40"    # 凡例のフォントの変更

# Lz=4
se yr [0:0.006]
se xl "k_B T/J"
se yl "f(4, T)"
se ou "FricDensP_Lz004.eps"
pl \
"dat/Lz004Lx0040Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=4, L_x=40", \
"dat/Lz004Lx0080Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=4, L_x=80", \
"dat/Lz004Lx0120Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=4, L_x=120", \
"dat/Lz004Lx0160Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=4, L_x=160", \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=4, L_x=200", \
"dat/Lz004Lx0040Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=4, L_x=40", \
"dat/Lz004Lx0080Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=4, L_x=80", \
"dat/Lz004Lx0120Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=4, L_x=120", \
"dat/Lz004Lx0160Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=4, L_x=160", \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=4, L_x=200"

se ou "FricDensD_Lz004.eps"
pl \
"dat/Lz004Lx0040Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Antiparallel, L_z=4, L_x=40", \
"dat/Lz004Lx0080Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Antiparallel, L_z=4, L_x=80", \
"dat/Lz004Lx0120Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Antiparallel, L_z=4, L_x=120", \
"dat/Lz004Lx0160Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Antiparallel, L_z=4, L_x=160", \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Antiparallel, L_z=4, L_x=200", \
"dat/Lz004Lx0040Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Parallel, L_z=4, L_x=40", \
"dat/Lz004Lx0080Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Parallel, L_z=4, L_x=80", \
"dat/Lz004Lx0120Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Parallel, L_z=4, L_x=120", \
"dat/Lz004Lx0160Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Parallel, L_z=4, L_x=160", \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Parallel, L_z=4, L_x=200"

se yr [-2:0]
se xl "k_B T/J"
se yl "e_b(4, T)"
se ou "EnDens_Lz004.eps"
pl \
"dat/Lz004Lx0040Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=4, L_x=40", \
"dat/Lz004Lx0080Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=4, L_x=80", \
"dat/Lz004Lx0120Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=4, L_x=120", \
"dat/Lz004Lx0160Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=4, L_x=160", \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=4, L_x=200", \
"dat/Lz004Lx0040Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=4, L_x=40", \
"dat/Lz004Lx0080Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=4, L_x=80", \
"dat/Lz004Lx0120Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=4, L_x=120", \
"dat/Lz004Lx0160Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=4, L_x=160", \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=4, L_x=200"

# Lz=6
se yr [0:0.006]
se xl "k_B T/J"
se yl "f(6, T)"
se ou "FricDensP_Lz006.eps"
pl \
"dat/Lz006Lx0060Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=6, L_x=60", \
"dat/Lz006Lx0120Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=6, L_x=120", \
"dat/Lz006Lx0180Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=6, L_x=180", \
"dat/Lz006Lx0240Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=6, L_x=240", \
"dat/Lz006Lx0300Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=6, L_x=300", \
"dat/Lz006Lx0060Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=6, L_x=60", \
"dat/Lz006Lx0120Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=6, L_x=120", \
"dat/Lz006Lx0180Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=6, L_x=180", \
"dat/Lz006Lx0240Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=6, L_x=240", \
"dat/Lz006Lx0300Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=6, L_x=300"

se ou "FricDensD_Lz006.eps"
pl \
"dat/Lz006Lx0060Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Antiparallel, L_z=6, L_x=60", \
"dat/Lz006Lx0120Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Antiparallel, L_z=6, L_x=120", \
"dat/Lz006Lx0180Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Antiparallel, L_z=6, L_x=180", \
"dat/Lz006Lx0240Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Antiparallel, L_z=6, L_x=240", \
"dat/Lz006Lx0300Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Antiparallel, L_z=6, L_x=300", \
"dat/Lz006Lx0060Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Parallel, L_z=6, L_x=60", \
"dat/Lz006Lx0120Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Parallel, L_z=6, L_x=120", \
"dat/Lz006Lx0180Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Parallel, L_z=6, L_x=180", \
"dat/Lz006Lx0240Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Parallel, L_z=6, L_x=240", \
"dat/Lz006Lx0300Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Parallel, L_z=6, L_x=300"

se yr [-2:0]
se xl "k_B T/J"
se yl "f(6, T)"
se ou "EnDens_Lz006.eps"
pl \
"dat/Lz006Lx0060Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=6, L_x=60", \
"dat/Lz006Lx0120Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=6, L_x=120", \
"dat/Lz006Lx0180Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=6, L_x=180", \
"dat/Lz006Lx0240Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=6, L_x=240", \
"dat/Lz006Lx0300Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=6, L_x=300", \
"dat/Lz006Lx0060Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=6, L_x=60", \
"dat/Lz006Lx0120Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=6, L_x=120", \
"dat/Lz006Lx0180Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=6, L_x=180", \
"dat/Lz006Lx0240Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=6, L_x=240", \
"dat/Lz006Lx0300Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=6, L_x=300"

# se ou "Results_Lz006_sub.eps"
# pl file1 u (1/$1):(($2-$8)/denom3):(($3+$9)/(180*10)) w er notit, \
# file1 u (1/$1):(($2-$8)/denom3) w l tit "Delta F, L_z=6, L_x=180"

# Lz=8
se yr [0:0.006]
se xl "k_B T/J"
se yl "f(8, T)"
se ou "FricDensP_Lz008.eps"
pl \
"dat/Lz008Lx0080Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=8, L_x=80", \
"dat/Lz008Lx0160Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=8, L_x=160", \
"dat/Lz008Lx0240Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=8, L_x=240", \
"dat/Lz008Lx0320Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=8, L_x=320", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=8, L_x=400", \
"dat/Lz008Lx0080Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=8, L_x=80", \
"dat/Lz008Lx0160Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=8, L_x=160", \
"dat/Lz008Lx0240Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=8, L_x=240", \
"dat/Lz008Lx0320Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=8, L_x=320", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=8, L_x=400"

se ou "FricDensD_Lz008.eps"
pl \
"dat/Lz008Lx0080Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Antiparallel, L_z=8, L_x=80", \
"dat/Lz008Lx0160Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Antiparallel, L_z=8, L_x=160", \
"dat/Lz008Lx0240Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Antiparallel, L_z=8, L_x=240", \
"dat/Lz008Lx0320Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Antiparallel, L_z=8, L_x=320", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Antiparallel, L_z=8, L_x=400", \
"dat/Lz008Lx0080Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Parallel, L_z=8, L_x=80", \
"dat/Lz008Lx0160Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Parallel, L_z=8, L_x=160", \
"dat/Lz008Lx0240Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Parallel, L_z=8, L_x=240", \
"dat/Lz008Lx0320Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Parallel, L_z=8, L_x=320", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Parallel, L_z=8, L_x=400"

se yr [-2:0]
se xl "k_B T/J"
se yl "e_b(8, T)"
se ou "EnDens_Lz008.eps"
pl \
"dat/Lz008Lx0080Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=8, L_x=80", \
"dat/Lz008Lx0160Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=8, L_x=160", \
"dat/Lz008Lx0240Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=8, L_x=240", \
"dat/Lz008Lx0320Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=8, L_x=320", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=8, L_x=400", \
"dat/Lz008Lx0080Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=8, L_x=80", \
"dat/Lz008Lx0160Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=8, L_x=160", \
"dat/Lz008Lx0240Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=8, L_x=240", \
"dat/Lz008Lx0320Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=8, L_x=320", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=8, L_x=400"

# se ou "Results_Lz008_sub.eps"
# pl file1 u (1/$1):(($2-$8)/denom3):(($3+$9)/(240*10)) w er notit, \
# file1 u (1/$1):(($2-$8)/denom3) w l tit "Delta F, L_z=8, L_x=240"

# Lz=10
se yr [0:0.006]
se xl "k_B T/J"
se yl "f(10, T)"
se ou "FricDensP_Lz010.eps"
pl \
"dat/Lz010Lx0100Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=10, L_x=100", \
"dat/Lz010Lx0200Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=10, L_x=200", \
"dat/Lz010Lx0300Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=10, L_x=300", \
"dat/Lz010Lx0400Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=10, L_x=400", \
"dat/Lz010Lx0500Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=10, L_x=500", \
"dat/Lz010Lx0100Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=10, L_x=100", \
"dat/Lz010Lx0200Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=10, L_x=200", \
"dat/Lz010Lx0300Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=10, L_x=300", \
"dat/Lz010Lx0400Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=10, L_x=400", \
"dat/Lz010Lx0500Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=10, L_x=500"

se ou "FricDensD_Lz010.eps"
pl \
"dat/Lz010Lx0100Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Antiparallel, L_z=10, L_x=100", \
"dat/Lz010Lx0200Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Antiparallel, L_z=10, L_x=200", \
"dat/Lz010Lx0300Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Antiparallel, L_z=10, L_x=300", \
"dat/Lz010Lx0400Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Antiparallel, L_z=10, L_x=400", \
"dat/Lz010Lx0500Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Antiparallel, L_z=10, L_x=500", \
"dat/Lz010Lx0100Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Parallel, L_z=10, L_x=100", \
"dat/Lz010Lx0200Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Parallel, L_z=10, L_x=200", \
"dat/Lz010Lx0300Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Parallel, L_z=10, L_x=300", \
"dat/Lz010Lx0400Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Parallel, L_z=10, L_x=400", \
"dat/Lz010Lx0500Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2):3 w er notit, "" u 1:(-$2) w l tit "Parallel, L_z=10, L_x=500"

se yr [-2:0]
se xl "k_B T/J"
se yl "e_b(10, T)"
se ou "EnDens_Lz010.eps"
pl \
"dat/Lz010Lx0100Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=10, L_x=100", \
"dat/Lz010Lx0200Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=10, L_x=200", \
"dat/Lz010Lx0300Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=10, L_x=300", \
"dat/Lz010Lx0400Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=10, L_x=400", \
"dat/Lz010Lx0500Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=10, L_x=500", \
"dat/Lz010Lx0100Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=10, L_x=100", \
"dat/Lz010Lx0200Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=10, L_x=200", \
"dat/Lz010Lx0300Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=10, L_x=300", \
"dat/Lz010Lx0400Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=10, L_x=400", \
"dat/Lz010Lx0500Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=10, L_x=500"

# se ou "Results_Lz10_sub.eps"
# pl \
# file1 u (1/$1):(($2-$8)/denom3):(($3+$9)/(300*10)) w er notit, \
# file1 u (1/$1):(($2-$8)/denom3) w l tit "Delta F, L_z=10, L_x=300"

# Lz=12
se yr [0:0.006]
se xl "k_B T/J"
se yl "f(12, T)"
se ou "FricDensP_Lz012.eps"
pl \
"dat/Lz012Lx0120Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=12, L_x=120", \
"dat/Lz012Lx0240Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=12, L_x=240", \
"dat/Lz012Lx0360Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=12, L_x=360", \
"dat/Lz012Lx0480Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=12, L_x=480", \
"dat/Lz012Lx0600Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=12, L_x=600", \
"dat/Lz012Lx0120Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=12, L_x=120", \
"dat/Lz012Lx0240Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=12, L_x=240", \
"dat/Lz012Lx0360Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=12, L_x=360", \
"dat/Lz012Lx0480Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=12, L_x=480", \
"dat/Lz012Lx0600Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=12, L_x=600"

se yr [-2:0]
se xl "k_B T/J"
se yl "e_b(12, T)"
se ou "EnDens_Lz012.eps"
pl \
"dat/Lz012Lx0120Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=12, L_x=120", \
"dat/Lz012Lx0240Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=12, L_x=240", \
"dat/Lz012Lx0360Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=12, L_x=360", \
"dat/Lz012Lx0480Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=12, L_x=480", \
"dat/Lz012Lx0600Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=12, L_x=600", \
"dat/Lz012Lx0120Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=12, L_x=120", \
"dat/Lz012Lx0240Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=12, L_x=240", \
"dat/Lz012Lx0360Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=12, L_x=360", \
"dat/Lz012Lx0480Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=12, L_x=480", \
"dat/Lz012Lx0600Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=12, L_x=600"

# se ou "Results_Lz12_sub.eps"
# pl file1 u (1/$1):(($2-$8)/denom3):(($3+$9)/(360*10)) w er notit, \
# file1 u (1/$1):(($2-$8)/denom3) w l tit "Delta F, L_z=12, L_x=360"

# Lz=14
se yr [0:0.006]
se xl "k_B T/J"
se yl "f(14, T)"
se ou "FricDensP_Lz014.eps"
pl \
"dat/Lz014Lx0140Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=14, L_x=140", \
"dat/Lz014Lx0280Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=14, L_x=280", \
"dat/Lz014Lx0420Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=14, L_x=420", \
"dat/Lz014Lx0560Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=14, L_x=560", \
"dat/Lz014Lx0700Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=14, L_x=700", \
"dat/Lz014Lx0140Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=14, L_x=140", \
"dat/Lz014Lx0280Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=14, L_x=280", \
"dat/Lz014Lx0420Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=14, L_x=420", \
"dat/Lz014Lx0560Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=14, L_x=560", \
"dat/Lz014Lx0700Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=14, L_x=700"

se yr [-2:0]
se xl "k_B T/J"
se yl "e_b(14, T)"
se ou "EnDens_Lz014.eps"
pl \
"dat/Lz014Lx0140Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=14, L_x=140", \
"dat/Lz014Lx0280Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=14, L_x=280", \
"dat/Lz014Lx0420Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=14, L_x=420", \
"dat/Lz014Lx0560Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=14, L_x=560", \
"dat/Lz014Lx0700Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=14, L_x=700", \
"dat/Lz014Lx0140Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=14, L_x=140", \
"dat/Lz014Lx0280Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=14, L_x=280", \
"dat/Lz014Lx0420Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=14, L_x=420", \
"dat/Lz014Lx0560Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=14, L_x=560", \
"dat/Lz014Lx0700Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=14, L_x=700"

# se ou "Results_Lz14_sub.eps"
# pl file1 u (1/$1):(($2-$8)/denom3):(($3+$9)/(420*10)) w er notit, \
# file1 u (1/$1):(($2-$8)/denom3) w l tit "Delta F, L_z=14, L_x=420"

# Lz=16
se yr [0:0.006]
se xl "k_B T/J"
se yl "f(16, T)"
se ou "FricDensP_Lz016.eps"
pl \
"dat/Lz016Lx0160Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=16, L_x=160", \
"dat/Lz016Lx0320Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=16, L_x=320", \
"dat/Lz016Lx0480Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=16, L_x=480", \
"dat/Lz016Lx0640Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=16, L_x=640", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=16, L_x=800", \
"dat/Lz016Lx0160Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=16, L_x=160", \
"dat/Lz016Lx0320Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=16, L_x=320", \
"dat/Lz016Lx0480Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=16, L_x=480", \
"dat/Lz016Lx0640Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=16, L_x=640", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=16, L_x=800"

se yr [-2.0:0.0]
se xl "k_B T/J"
se yl "e_b(16, T)"
se ou "EnDens_Lz016.eps"
pl \
"dat/Lz016Lx0160Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=16, L_x=160", \
"dat/Lz016Lx0320Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=16, L_x=320", \
"dat/Lz016Lx0480Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=16, L_x=480", \
"dat/Lz016Lx0640Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=16, L_x=640", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=16, L_x=800", \
"dat/Lz016Lx0160Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=16, L_x=160", \
"dat/Lz016Lx0320Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=16, L_x=320", \
"dat/Lz016Lx0480Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=16, L_x=480", \
"dat/Lz016Lx0640Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=16, L_x=640", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=16, L_x=800"

# Lz=32
se yr [0:0.006]
se xl "k_B T/J"
se yl "f(32, T)"
se ou "FricDensP_Lz032.eps"
pl \
"dat/Lz032Lx0960Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=32, L_x=960", \
"dat/Lz032Lx1280Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=32, L_x=1280", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=32, L_x=1600", \
"dat/Lz032Lx0960Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=32, L_x=960", \
"dat/Lz032Lx1280Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=32, L_x=1280", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=32, L_x=1600"

se yr [-2.0:0.0]
se xl "k_B T/J"
se yl "e_b(32, T)"
se ou "EnDens_Lz032.eps"
pl \
"dat/Lz032Lx0960Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=32, L_x=960", \
"dat/Lz032Lx1280Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=32, L_x=1280", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=32, L_x=1600", \
"dat/Lz032Lx0960Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=32, L_x=060", \
"dat/Lz032Lx1280Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=32, L_x=1280", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=32, L_x=1600"

# Lz=64
se yr [0:0.006]
se xl "k_B T/J"
se yl "f(64, T)"
se ou "FricDensP_Lz064.eps"
pl \
"dat/Lz064Lx1920Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=64, L_x=1920", \
"dat/Lz064Lx2560Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=64, L_x=2560", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=64, L_x=3200", \
"dat/Lz064Lx1920Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=64, L_x=1920", \
"dat/Lz064Lx2560Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=64, L_x=2560", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_pump_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=64, L_x=3200"

se yr [-2.0:0.0]
se xl "k_B T/J"
se yl "e_b(64, T)"
se ou "EnDens_Lz064.eps"
pl \
"dat/Lz064Lx1920Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=64, L_x=1920", \
"dat/Lz064Lx2560Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=64, L_x=2560", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Antiparallel, L_z=64, L_x=3200", \
"dat/Lz064Lx1920Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=64, L_x=1920", \
"dat/Lz064Lx2560Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=64, L_x=2560", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_energy_T.dat" u 1:2:3 w er notit, "" u 1:2 w l tit "Parallel, L_z=64, L_x=3200"

# se ou "Results_Lz16_sub.eps"
# pl file1 u (1/$1):(($2-$8)/denom3):(($3+$9)/(480*10)) w er notit, \
# file1 u (1/$1):(($2-$8)/denom3) w l tit "Delta F, L_z=16, L_x=480"
#
se k r b
se xl "k_B T/J"
se yl "f(L_z, T)"
se yr [0.000:0.006]
se ou "FricDensP_Allsize_AP.eps"
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "L_z=4", \
"dat/Lz006Lx0300Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "L_z=6", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "L_z=8", \
"dat/Lz010Lx0500Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "L_z=10", \
"dat/Lz012Lx0600Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "L_z=12", \
"dat/Lz014Lx0700Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "L_z=14", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "L_z=16", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "L_z=32", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "L_z=64"

se k r b
se xl "k_B T/J"
se yl "f(L_z, T)"
se yr [0.000:0.006]
se ou "FricDensP_Allsize_P.eps"
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "L_z=4", \
"dat/Lz006Lx0300Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "L_z=6", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "L_z=8", \
"dat/Lz010Lx0500Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "L_z=10", \
"dat/Lz012Lx0600Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "L_z=12", \
"dat/Lz014Lx0700Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "L_z=14", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "L_z=16", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "L_z=32", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "L_z=64"

se k r b
se xl "k_B T/J"
se yl "e_b(L_z, T)"
se ou "EnDens_Allsize_AP.eps"
se yr [-2.0:0.0]
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "L_z=4", \
"dat/Lz006Lx0300Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "L_z=6", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "L_z=8", \
"dat/Lz010Lx0500Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "L_z=10", \
"dat/Lz012Lx0600Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "L_z=12", \
"dat/Lz014Lx0700Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "L_z=14", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "L_z=16", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "L_z=32", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "L_z=64"

se k r b
se xl "k_B T/J"
se yl "e_b(L_z, T)"
se ou "EnDens_Allsize_P.eps"
se yr [-2.0:0.0]
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "L_z=4", \
"dat/Lz006Lx0300Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "L_z=6", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "L_z=8", \
"dat/Lz010Lx0500Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "L_z=10", \
"dat/Lz012Lx0600Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "L_z=12", \
"dat/Lz014Lx0700Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "L_z=14", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "L_z=16", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "L_z=32", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "L_z=64"

se k r t
se xl "k_B T/J"
se yl "c_b(L_z, T)"
se ou "dEnDens_Allsize_AP.eps"
se yr [0.0:2.0]
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "L_z=4", \
"dat/Lz006Lx0300Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "L_z=6", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "L_z=8", \
"dat/Lz010Lx0500Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "L_z=10", \
"dat/Lz012Lx0600Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "L_z=12", \
"dat/Lz014Lx0700Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "L_z=14", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "L_z=16", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "L_z=32", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "L_z=64"

se k r t
se xl "k_B T/J"
se yl "c_b(L_z, T)"
se ou "dEnDens_Allsize_P.eps"
se yr [0.0:2.0]
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "L_z=4", \
"dat/Lz006Lx0300Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "L_z=6", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "L_z=8", \
"dat/Lz010Lx0500Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "L_z=10", \
"dat/Lz012Lx0600Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "L_z=12", \
"dat/Lz014Lx0700Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "L_z=14", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "L_z=16", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "L_z=32", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "L_z=64"

se yr [-0.002:0.04]

se k r t
se xl "k_B T/J"
se yl "df/dT(L_z, T)"
se ou "dFricDensP_Allsize_AP.eps"
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "L_z=4", \
"dat/Lz006Lx0300Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "L_z=6", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "L_z=8", \
"dat/Lz010Lx0500Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "L_z=10", \
"dat/Lz012Lx0600Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "L_z=12", \
"dat/Lz014Lx0700Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "L_z=14", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "L_z=16", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "L_z=32", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "L_z=64"

se k r t
se xl "k_B T/J"
se yl "df/dT(L_z, T)"
se ou "dFricDensP_Allsize_P.eps"
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "L_z=4", \
"dat/Lz006Lx0300Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "L_z=6", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "L_z=8", \
"dat/Lz010Lx0500Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "L_z=10", \
"dat/Lz012Lx0600Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "L_z=12", \
"dat/Lz014Lx0700Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "L_z=14", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "L_z=16", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "L_z=32", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "L_z=64"

se k r t
se ou "dFricDensP_Allsize_AP_Aligned.eps"
Tc04=1.35; Tc06=1.65; Tc08=1.85; Tc10=2.05; Tc12=2.07; Tc14=2.21; Tc16=2.25
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_dPump_T.dat" u ($1-Tc04):2 w lp lw 3 tit "L_z=4", \
"dat/Lz006Lx0300Ly__Vel10/antiparallel/res_dPump_T.dat" u ($1-Tc06):2 w lp lw 3 tit "L_z=6", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_dPump_T.dat" u ($1-Tc08):2 w lp lw 3 tit "L_z=8", \
"dat/Lz010Lx0500Ly__Vel10/antiparallel/res_dPump_T.dat" u ($1-Tc10):2 w lp lw 3 tit "L_z=10", \
"dat/Lz012Lx0600Ly__Vel10/antiparallel/res_dPump_T.dat" u ($1-Tc12):2 w lp lw 3 tit "L_z=12", \
"dat/Lz014Lx0700Ly__Vel10/antiparallel/res_dPump_T.dat" u ($1-Tc14):2 w lp lw 3 tit "L_z=14", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_dPump_T.dat" u ($1-Tc16):2 w lp lw 3 tit "L_z=16", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_dPump_T.dat" u ($1-Tc16):2 w lp lw 3 tit "L_z=32", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_dPump_T.dat" u ($1-Tc16):2 w lp lw 3 tit "L_z=64"

se k r t
se ou "dFricDensP_Allsize_P_Aligned.eps"
Tc04=2.65343; Tc06=2.55208; Tc08=2.4907; Tc10=2.4907; Tc12=2.4907; Tc14=2.45402; Tc16=2.45402
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_dPump_T.dat" u ($1-Tc04):2 w lp lw 3 tit "L_z=4", \
"dat/Lz006Lx0300Ly__Vel10/parallel/res_dPump_T.dat" u ($1-Tc06):2 w lp lw 3 tit "L_z=6", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_dPump_T.dat" u ($1-Tc08):2 w lp lw 3 tit "L_z=8", \
"dat/Lz010Lx0500Ly__Vel10/parallel/res_dPump_T.dat" u ($1-Tc10):2 w lp lw 3 tit "L_z=10", \
"dat/Lz012Lx0600Ly__Vel10/parallel/res_dPump_T.dat" u ($1-Tc12):2 w lp lw 3 tit "L_z=12", \
"dat/Lz014Lx0700Ly__Vel10/parallel/res_dPump_T.dat" u ($1-Tc14):2 w lp lw 3 tit "L_z=14", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_dPump_T.dat" u ($1-Tc16):2 w lp lw 3 tit "L_z=16", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_dPump_T.dat" u ($1-Tc16):2 w lp lw 3 tit "L_z=32", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_dPump_T.dat" u ($1-Tc16):2 w lp lw 3 tit "L_z=64"

se k r t
se ou "dFricDensP_Allsize_AP_Aligned_HTScaled.eps"
se xr [0:30]
se yr [-0.0005:0.0100]
Tc04=1.35; Tc06=1.65; Tc08=1.85; Tc10=2.05; Tc12=2.07; Tc14=2.21; Tc16=2.25; a=-0.25
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_dPump_T.dat" u (4*($1-Tc04)):(4**a*$2) w lp lw 3 tit "L_z=4", \
"dat/Lz006Lx0300Ly__Vel10/antiparallel/res_dPump_T.dat" u (6*($1-Tc06)):(6**a*$2) w lp lw 3 tit "L_z=6", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_dPump_T.dat" u (8*($1-Tc08)):(8**a*$2) w lp lw 3 tit "L_z=8", \
"dat/Lz010Lx0500Ly__Vel10/antiparallel/res_dPump_T.dat" u (10*($1-Tc10)):(10**a*$2) w lp lw 3 tit "L_z=10", \
"dat/Lz012Lx0600Ly__Vel10/antiparallel/res_dPump_T.dat" u (12*($1-Tc12)):(12**a*$2) w lp lw 3 tit "L_z=12", \
"dat/Lz014Lx0700Ly__Vel10/antiparallel/res_dPump_T.dat" u (14*($1-Tc14)):(14**a*$2) w lp lw 3 tit "L_z=14", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_dPump_T.dat" u (16*($1-Tc16)):(16**a*$2) w lp lw 3 tit "L_z=16", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_dPump_T.dat" u (16*($1-Tc16)):(16**a*$2) w lp lw 3 tit "L_z=32", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_dPump_T.dat" u (16*($1-Tc16)):(16**a*$2) w lp lw 3 tit "L_z=64"

se k r t
se ou "dFricDensP_Allsize_P_Aligned_HTScaled.eps"
se xr [0:30]
se yr [-0.0005:0.0015]
Tc04=2.65343; Tc06=2.55208; Tc08=2.4907; Tc10=2.4907; Tc12=2.4907; Tc14=2.45402; Tc16=2.45402; a=-1.0
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_dPump_T.dat" u (4*($1-Tc04)):(4**a*$2) w lp lw 3 tit "L_z=4", \
"dat/Lz006Lx0300Ly__Vel10/parallel/res_dPump_T.dat" u (6*($1-Tc06)):(6**a*$2) w lp lw 3 tit "L_z=6", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_dPump_T.dat" u (8*($1-Tc08)):(8**a*$2) w lp lw 3 tit "L_z=8", \
"dat/Lz010Lx0500Ly__Vel10/parallel/res_dPump_T.dat" u (10*($1-Tc10)):(10**a*$2) w lp lw 3 tit "L_z=10", \
"dat/Lz012Lx0600Ly__Vel10/parallel/res_dPump_T.dat" u (12*($1-Tc12)):(12**a*$2) w lp lw 3 tit "L_z=12", \
"dat/Lz014Lx0700Ly__Vel10/parallel/res_dPump_T.dat" u (14*($1-Tc14)):(14**a*$2) w lp lw 3 tit "L_z=14", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_dPump_T.dat" u (16*($1-Tc16)):(16**a*$2) w lp lw 3 tit "L_z=16", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_dPump_T.dat" u (16*($1-Tc16)):(16**a*$2) w lp lw 3 tit "L_z=32", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_dPump_T.dat" u (16*($1-Tc16)):(16**a*$2) w lp lw 3 tit "L_z=64"

se k r t
se ou "dFricDensP_Allsize_AP_Aligned_LTScaled.eps"
se xr [-3:0]
se yr [-0.0005:0.0100]
Tc04=1.35; Tc06=1.65; Tc08=1.85; Tc10=2.05; Tc12=2.07; Tc14=2.21; Tc16=2.25; a=0.0
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_dPump_T.dat" u (($1-Tc04)):(4**a*$2) w lp lw 3 tit "L_z=4", \
"dat/Lz006Lx0300Ly__Vel10/antiparallel/res_dPump_T.dat" u (($1-Tc06)):(6**a*$2) w lp lw 3 tit "L_z=6", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_dPump_T.dat" u (($1-Tc08)):(8**a*$2) w lp lw 3 tit "L_z=8", \
"dat/Lz010Lx0500Ly__Vel10/antiparallel/res_dPump_T.dat" u (($1-Tc10)):(10**a*$2) w lp lw 3 tit "L_z=10", \
"dat/Lz012Lx0600Ly__Vel10/antiparallel/res_dPump_T.dat" u (($1-Tc12)):(12**a*$2) w lp lw 3 tit "L_z=12", \
"dat/Lz014Lx0700Ly__Vel10/antiparallel/res_dPump_T.dat" u (($1-Tc14)):(14**a*$2) w lp lw 3 tit "L_z=14", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_dPump_T.dat" u (($1-Tc16)):(16**a*$2) w lp lw 3 tit "L_z=16", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_dPump_T.dat" u (($1-Tc16)):(16**a*$2) w lp lw 3 tit "L_z=32", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_dPump_T.dat" u (($1-Tc16)):(16**a*$2) w lp lw 3 tit "L_z=64"

se k r t
se ou "dFricDensP_Allsize_P_Aligned_LTScaled.eps"
se xr [-3:0]
se yr [-0.0005:0.0100]
Tc04=2.65343; Tc06=2.55208; Tc08=2.4907; Tc10=2.4907; Tc12=2.4907; Tc14=2.45402; Tc16=2.45402; a=0.0
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_dPump_T.dat" u (($1-Tc04)):(4**a*$2) w lp lw 3 tit "L_z=4", \
"dat/Lz006Lx0300Ly__Vel10/parallel/res_dPump_T.dat" u (($1-Tc06)):(6**a*$2) w lp lw 3 tit "L_z=6", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_dPump_T.dat" u (($1-Tc08)):(8**a*$2) w lp lw 3 tit "L_z=8", \
"dat/Lz010Lx0500Ly__Vel10/parallel/res_dPump_T.dat" u (($1-Tc10)):(10**a*$2) w lp lw 3 tit "L_z=10", \
"dat/Lz012Lx0600Ly__Vel10/parallel/res_dPump_T.dat" u (($1-Tc12)):(12**a*$2) w lp lw 3 tit "L_z=12", \
"dat/Lz014Lx0700Ly__Vel10/parallel/res_dPump_T.dat" u (($1-Tc14)):(14**a*$2) w lp lw 3 tit "L_z=14", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_dPump_T.dat" u (($1-Tc16)):(16**a*$2) w lp lw 3 tit "L_z=16", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_dPump_T.dat" u (($1-Tc16)):(16**a*$2) w lp lw 3 tit "L_z=32", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_dPump_T.dat" u (($1-Tc16)):(16**a*$2) w lp lw 3 tit "L_z=64"
