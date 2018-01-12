set terminal epslatex standalone color #standaloneを追加
se gr
se k below right
se k box

set border 3
set xtics nomirror
set ytics nomirror

set tics font ",20"   # 目盛りのフォントの変更
set xlabel font ",24" # xlabelのフォントの変更
set ylabel font ",24" # ylabelのフォントの変更
set key font ",20"    # 凡例のフォントの変更

# Lz=4
se yr [0:0.006]
se xl "$k_{\\rm B}T/J$"
se yl "$f(L_{z}, T)$"
se ou "FricDensP_Lz004.tex"
pl \
"dat/Lz004Lx0120Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=120$", \
"dat/Lz004Lx0160Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=160$", \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=200$", \
"dat/Lz004Lx0120Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=120$", \
"dat/Lz004Lx0160Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=160$", \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=200$"

se ou "FricDensD_Lz004.tex"
pl \
"dat/Lz004Lx0120Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2) w lp lw 3 tit  "Antiparallel, $L_{x}=120$", \
"dat/Lz004Lx0160Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2) w lp lw 3 tit  "Antiparallel, $L_{x}=160$", \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2) w lp lw 3 tit  "Antiparallel, $L_{x}=200$", \
"dat/Lz004Lx0120Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2) w lp lw 3 tit  "Parallel, $L_{x}=120$", \
"dat/Lz004Lx0160Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2) w lp lw 3 tit  "Parallel, $L_{x}=160$", \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2) w lp lw 3 tit  "Parallel, $L_{x}=200$"

se yr [-2:0]
se xl "$k_{\\rm B}T/J$"
se yl "$\\epsilon_{\\rm B}(L_{z}, T)$"
se ou "EnDens_Lz004.tex"
pl \
"dat/Lz004Lx0120Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=120$", \
"dat/Lz004Lx0160Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=160$", \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=200$", \
"dat/Lz004Lx0120Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=120$", \
"dat/Lz004Lx0160Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=160$", \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=200$"

# Lz=6
se yr [0:0.006]
se xl "$k_{\\rm B}T/J$"
se yl "$f(L_{z}, T)$"
se ou "FricDensP_Lz006.tex"
pl \
"dat/Lz006Lx0180Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=180$", \
"dat/Lz006Lx0240Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=240$", \
"dat/Lz006Lx0300Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=300$", \
"dat/Lz006Lx0180Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=180$", \
"dat/Lz006Lx0240Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=240$", \
"dat/Lz006Lx0300Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=300$"

se ou "FricDensD_Lz006.tex"
pl \
"dat/Lz006Lx0180Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2) w lp lw 3 tit  "Antiparallel, $L_{x}=180$", \
"dat/Lz006Lx0240Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2) w lp lw 3 tit  "Antiparallel, $L_{x}=240$", \
"dat/Lz006Lx0300Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2) w lp lw 3 tit  "Antiparallel, $L_{x}=300$", \
"dat/Lz006Lx0180Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2) w lp lw 3 tit  "Parallel, $L_{x}=180$", \
"dat/Lz006Lx0240Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2) w lp lw 3 tit  "Parallel, $L_{x}=240$", \
"dat/Lz006Lx0300Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2) w lp lw 3 tit  "Parallel, $L_{x}=300$"

se yr [-2:0]
se xl "$k_{\\rm B}T/J$"
se yl "$f(L_{z}, T)$"
se ou "EnDens_Lz006.tex"
pl \
"dat/Lz006Lx0180Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=180$", \
"dat/Lz006Lx0240Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=240$", \
"dat/Lz006Lx0300Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=300$", \
"dat/Lz006Lx0180Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=180$", \
"dat/Lz006Lx0240Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=240$", \
"dat/Lz006Lx0300Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=300$"

# se ou "Results_Lz006_sub.tex"
# pl file1 u (1/$1):(($2-$8)/denom3):(($3+$9)/(180*10)) w er notit, \
# file1 u (1/$1):(($2-$8)/denom3) w l tit "Delta F, $L_{x}=180$"

# Lz=8
se yr [0:0.006]
se xl "$k_{\\rm B}T/J$"
se yl "$f(L_{z}, T)$"
se ou "FricDensP_Lz008.tex"
pl \
"dat/Lz008Lx0240Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=240$", \
"dat/Lz008Lx0320Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=320$", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=400$", \
"dat/Lz008Lx0240Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=240$", \
"dat/Lz008Lx0320Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=320$", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=400$"

se ou "FricDensD_Lz008.tex"
pl \
"dat/Lz008Lx0240Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2) w lp lw 3 tit  "Antiparallel, $L_{x}=240$", \
"dat/Lz008Lx0320Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2) w lp lw 3 tit  "Antiparallel, $L_{x}=320$", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2) w lp lw 3 tit  "Antiparallel, $L_{x}=400$", \
"dat/Lz008Lx0240Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2) w lp lw 3 tit  "Parallel, $L_{x}=240$", \
"dat/Lz008Lx0320Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2) w lp lw 3 tit  "Parallel, $L_{x}=320$", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2) w lp lw 3 tit  "Parallel, $L_{x}=400$"

se yr [-2:0]
se xl "$k_{\\rm B}T/J$"
se yl "$\\epsilon_{\\rm B}(L_{z}, T)$"
se ou "EnDens_Lz008.tex"
pl \
"dat/Lz008Lx0240Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=240$", \
"dat/Lz008Lx0320Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=320$", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=400$", \
"dat/Lz008Lx0240Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=240$", \
"dat/Lz008Lx0320Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=320$", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=400$"

# se ou "Results_Lz008_sub.tex"
# pl file1 u (1/$1):(($2-$8)/denom3):(($3+$9)/(240*10)) w er notit, \
# file1 u (1/$1):(($2-$8)/denom3) w l tit "Delta F, $L_{x}=240$"

# Lz=10
se yr [0:0.006]
se xl "$k_{\\rm B}T/J$"
se yl "$f(L_{z}, T)$"
se ou "FricDensP_Lz010.tex"
pl \
"dat/Lz010Lx0300Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=300$", \
"dat/Lz010Lx0400Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=400$", \
"dat/Lz010Lx0500Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=500$", \
"dat/Lz010Lx0300Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=300$", \
"dat/Lz010Lx0400Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=400$", \
"dat/Lz010Lx0500Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=500$"

se ou "FricDensD_Lz010.tex"
pl \
"dat/Lz010Lx0300Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2) w lp lw 3 tit  "Antiparallel, $L_{x}=300$", \
"dat/Lz010Lx0400Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2) w lp lw 3 tit  "Antiparallel, $L_{x}=400$", \
"dat/Lz010Lx0500Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2) w lp lw 3 tit  "Antiparallel, $L_{x}=500$", \
"dat/Lz010Lx0300Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2) w lp lw 3 tit  "Parallel, $L_{x}=300$", \
"dat/Lz010Lx0400Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2) w lp lw 3 tit  "Parallel, $L_{x}=400$", \
"dat/Lz010Lx0500Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2) w lp lw 3 tit  "Parallel, $L_{x}=500$"

se yr [-2:0]
se xl "$k_{\\rm B}T/J$"
se yl "$\\epsilon_{\\rm B}(L_{z}, T)$"
se ou "EnDens_Lz010.tex"
pl \
"dat/Lz010Lx0300Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=300$", \
"dat/Lz010Lx0400Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=400$", \
"dat/Lz010Lx0500Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=500$", \
"dat/Lz010Lx0300Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=300$", \
"dat/Lz010Lx0400Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=400$", \
"dat/Lz010Lx0500Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=500$"

# se ou "Results_Lz10_sub.tex"
# pl \
# file1 u (1/$1):(($2-$8)/denom3):(($3+$9)/(300*10)) w er notit, \
# file1 u (1/$1):(($2-$8)/denom3) w l tit "Delta F, $L_{x}=300$"

# Lz=12
se yr [0:0.006]
se xl "$k_{\\rm B}T/J$"
se yl "$f(L_{z}, T)$"
se ou "FricDensP_Lz012.tex"
pl \
"dat/Lz012Lx0360Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=360$", \
"dat/Lz012Lx0480Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=480$", \
"dat/Lz012Lx0600Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=600$", \
"dat/Lz012Lx0360Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=360$", \
"dat/Lz012Lx0480Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=480$", \
"dat/Lz012Lx0600Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=600$"

se yr [-2:0]
se xl "$k_{\\rm B}T/J$"
se yl "$\\epsilon_{\\rm B}(L_{z}, T)$"
se ou "EnDens_Lz012.tex"
pl \
"dat/Lz012Lx0360Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=360$", \
"dat/Lz012Lx0480Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=480$", \
"dat/Lz012Lx0600Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=600$", \
"dat/Lz012Lx0360Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=360$", \
"dat/Lz012Lx0480Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=480$", \
"dat/Lz012Lx0600Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=600$"

# se ou "Results_Lz12_sub.tex"
# pl file1 u (1/$1):(($2-$8)/denom3):(($3+$9)/(360*10)) w er notit, \
# file1 u (1/$1):(($2-$8)/denom3) w l tit "Delta F, $L_{x}=360$"

# Lz=14
se yr [0:0.006]
se xl "$k_{\\rm B}T/J$"
se yl "$f(L_{z}, T)$"
se ou "FricDensP_Lz014.tex"
pl \
"dat/Lz014Lx0420Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=420$", \
"dat/Lz014Lx0560Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=560$", \
"dat/Lz014Lx0700Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=700$", \
"dat/Lz014Lx0420Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=420$", \
"dat/Lz014Lx0560Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=560$", \
"dat/Lz014Lx0700Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=700$"

se yr [-2:0]
se xl "$k_{\\rm B}T/J$"
se yl "$\\epsilon_{\\rm B}(L_{z}, T)$"
se ou "EnDens_Lz014.tex"
pl \
"dat/Lz014Lx0420Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=420$", \
"dat/Lz014Lx0560Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=560$", \
"dat/Lz014Lx0700Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=700$", \
"dat/Lz014Lx0420Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=420$", \
"dat/Lz014Lx0560Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=560$", \
"dat/Lz014Lx0700Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=700$"

# se ou "Results_Lz14_sub.tex"
# pl file1 u (1/$1):(($2-$8)/denom3):(($3+$9)/(420*10)) w er notit, \
# file1 u (1/$1):(($2-$8)/denom3) w l tit "Delta F, $L_{x}=420$"

# Lz=16
se yr [0:0.006]
se xl "$k_{\\rm B}T/J$"
se yl "$f(L_{z}, T)$"
se ou "FricDensP_Lz016.tex"
pl \
"dat/Lz016Lx0480Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=480$", \
"dat/Lz016Lx0640Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=640$", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=800$", \
"dat/Lz016Lx0480Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=480$", \
"dat/Lz016Lx0640Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=640$", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=800$"

se yr [-2.0:0.0]
se xl "$k_{\\rm B}T/J$"
se yl "$\\epsilon_{\\rm B}(L_{z}, T)$"
se ou "EnDens_Lz016.tex"
pl \
"dat/Lz016Lx0480Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=480$", \
"dat/Lz016Lx0640Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=640$", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=800$", \
"dat/Lz016Lx0480Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=480$", \
"dat/Lz016Lx0640Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=640$", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=800$"

# Lz=32
se yr [0:0.006]
se xl "$k_{\\rm B}T/J$"
se yl "f(32, T)"
se ou "FricDensP_Lz032.tex"
pl \
"dat/Lz032Lx0960Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=960$", \
"dat/Lz032Lx1280Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=1280$", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=1600$", \
"dat/Lz032Lx0960Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=960$", \
"dat/Lz032Lx1280Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=1280$", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=1600$"

se yr [-2.0:0.0]
se xl "$k_{\\rm B}T/J$"
se yl "{/Symbol e}_b(32, T)"
se ou "EnDens_Lz032.tex"
pl \
"dat/Lz032Lx0960Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=960$", \
"dat/Lz032Lx1280Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=1280$", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=1600$", \
"dat/Lz032Lx0960Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=060$", \
"dat/Lz032Lx1280Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=1280$", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=1600$"

# Lz=64
se yr [0:0.006]
se xl "$k_{\\rm B}T/J$"
se yl "f(64, T)"
se ou "FricDensP_Lz064.tex"
pl \
"dat/Lz064Lx1920Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=1920$", \
"dat/Lz064Lx2560Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=2560$", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=3200$", \
"dat/Lz064Lx1920Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=1920$", \
"dat/Lz064Lx2560Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=2560$", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=3200$"

se yr [-2.0:0.0]
se xl "$k_{\\rm B}T/J$"
se yl "{/Symbol e}_b(64, T)"
se ou "EnDens_Lz064.tex"
pl \
"dat/Lz064Lx1920Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=1920$", \
"dat/Lz064Lx2560Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=2560$", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Antiparallel, $L_{x}=3200$", \
"dat/Lz064Lx1920Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=1920$", \
"dat/Lz064Lx2560Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=2560$", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit  "Parallel, $L_{x}=3200$"

# se ou "Results_Lz16_sub.tex"
# pl file1 u (1/$1):(($2-$8)/denom3):(($3+$9)/(480*10)) w er notit, \
# file1 u (1/$1):(($2-$8)/denom3) w l tit "Delta F, $L_{x}=480$"
#
se k r b
se xl "$k_{\\rm B}T/J$"
se yl "$f(L_{z}, T)$"
se yr [0.000:0.006]
se ou "FricDensP_Allsize_AP.tex"
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=4$", \
"dat/Lz006Lx0300Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz010Lx0500Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=10$", \
"dat/Lz012Lx0600Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=12$", \
"dat/Lz014Lx0700Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=14$", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=64$"

se k r b
se xl "$k_{\\rm B}T/J$"
se yl "$f(L_{z}, T)$"
se yr [0.000:0.006]
se ou "FricDensP_Allsize_AP_.tex"
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=4$", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=64$"

se k r b
se xl "$k_{\\rm B}T/J$"
se yl "$f(L_{z}, T)$"
se yr [0.000:0.006]
se ou "FricDensP_Allsize_P.tex"
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=4$", \
"dat/Lz006Lx0300Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz010Lx0500Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=10$", \
"dat/Lz012Lx0600Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=12$", \
"dat/Lz014Lx0700Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=14$", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=64$"

se k r b
se xl "$k_{\\rm B}T/J$"
se yl "$f(L_{z}, T)$"
se yr [0.000:0.006]
se ou "FricDensP_Allsize_P_.tex"
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=4$", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 3 tit "$L_z=64$"

se k r b
se xl "$k_{\\rm B}T/J$"
se yl "$\\epsilon_{\\rm B}(L_{z}, T)$"
se ou "EnDens_Allsize_AP.tex"
se yr [-2.0:0.0]
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=4$", \
"dat/Lz006Lx0300Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz010Lx0500Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=10$", \
"dat/Lz012Lx0600Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=12$", \
"dat/Lz014Lx0700Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=14$", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=64$"

se k r b
se xl "$k_{\\rm B}T/J$"
se yl "$\\epsilon_{\\rm B}(L_{z}, T)$"
se ou "EnDens_Allsize_AP_.tex"
se yr [-2.0:0.0]
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=4$", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=64$"

se k r b
se xl "$k_{\\rm B}T/J$"
se yl "$\\epsilon_{\\rm B}(L_{z}, T)$"
se ou "EnDens_Allsize_P.tex"
se yr [-2.0:0.0]
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=4$", \
"dat/Lz006Lx0300Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz010Lx0500Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=10$", \
"dat/Lz012Lx0600Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=12$", \
"dat/Lz014Lx0700Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=14$", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=64$"

se k r b
se xl "$k_{\\rm B}T/J$"
se yl "$\\epsilon_{\\rm B}(L_{z}, T)$"
se ou "EnDens_Allsize_P_.tex"
se yr [-2.0:0.0]
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=4$", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 3 tit "$L_z=64$"

se k r t
se xl "$k_{\\rm B}T/J$"
se yl "$c_{\\rm b}(L_z, T)$"
se ou "dEnDens_Allsize_AP.tex"
se yr [0.0:2.0]
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=4$", \
"dat/Lz006Lx0300Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz010Lx0500Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=10$", \
"dat/Lz012Lx0600Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=12$", \
"dat/Lz014Lx0700Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=14$", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=64$"

se k r t
se xl "$k_{\\rm B}T/J$"
se yl "$c_{\\rm b}(L_z, T)$"
se ou "dEnDens_Allsize_AP_.tex"
se yr [0.0:2.0]
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=4$", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=64$"

se k r t
se xl "$k_{\\rm B}T/J$"
se yl "$c_{\\rm b}(L_z, T)$"
se ou "dEnDens_Allsize_P.tex"
se yr [0.0:2.0]
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=4$", \
"dat/Lz006Lx0300Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz010Lx0500Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=10$", \
"dat/Lz012Lx0600Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=12$", \
"dat/Lz014Lx0700Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=14$", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=64$"

se k r t
se xl "$k_{\\rm B}T/J$"
se yl "$c_{\\rm b}(L_z, T)$"
se ou "dEnDens_Allsize_P_.tex"
se yr [0.0:2.0]
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=4$", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 3 tit "$L_z=64$"

se yr [-0.002:0.04]

se k r t
se xl "$k_{\\rm B}T/J$"
se yl "$df/dT(L_{z}, T)$"
se ou "dFricDensP_Allsize_AP.tex"
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=4$", \
"dat/Lz006Lx0300Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz010Lx0500Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=10$", \
"dat/Lz012Lx0600Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=12$", \
"dat/Lz014Lx0700Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=14$", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=64$"

se k r t
se xl "$k_{\\rm B}T/J$"
se yl "$df/dT(L_{z}, T)$"
se ou "dFricDensP_Allsize_AP_.tex"
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=4$", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=64$"

se k r t
se xl "$k_{\\rm B}T/J$"
se yl "$df/dT(L_{z}, T)$"
se ou "dFricDensP_Allsize_P.tex"
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=4$", \
"dat/Lz006Lx0300Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz010Lx0500Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=10$", \
"dat/Lz012Lx0600Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=12$", \
"dat/Lz014Lx0700Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=14$", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=64$"

se k r t
se xl "$k_{\\rm B}T/J$"
se yl "$df/dT(L_{z}, T)$"
se ou "dFricDensP_Allsize_P_.tex"
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=4$", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 3 tit "$L_z=64$"

se k r t
se ou "dFricDensP_Allsize_AP_Aligned.tex"
Tc04=1.35; Tc06=1.65; Tc08=1.85; Tc10=2.05; Tc12=2.07; Tc14=2.21; Tc16=2.25
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_dPump_T.dat" u ($1-Tc04):2 w lp lw 3 tit "$L_z=4$", \
"dat/Lz006Lx0300Ly__Vel10/antiparallel/res_dPump_T.dat" u ($1-Tc06):2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_dPump_T.dat" u ($1-Tc08):2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz010Lx0500Ly__Vel10/antiparallel/res_dPump_T.dat" u ($1-Tc10):2 w lp lw 3 tit "$L_z=10$", \
"dat/Lz012Lx0600Ly__Vel10/antiparallel/res_dPump_T.dat" u ($1-Tc12):2 w lp lw 3 tit "$L_z=12$", \
"dat/Lz014Lx0700Ly__Vel10/antiparallel/res_dPump_T.dat" u ($1-Tc14):2 w lp lw 3 tit "$L_z=14$", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_dPump_T.dat" u ($1-Tc16):2 w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_dPump_T.dat" u ($1-Tc16):2 w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_dPump_T.dat" u ($1-Tc16):2 w lp lw 3 tit "$L_z=64$"

se k r t
se ou "dFricDensP_Allsize_AP_Aligned_.tex"
Tc04=1.35; Tc06=1.65; Tc08=1.85; Tc10=2.05; Tc12=2.07; Tc14=2.21; Tc16=2.25
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_dPump_T.dat" u ($1-Tc04):2 w lp lw 3 tit "$L_z=4$", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_dPump_T.dat" u ($1-Tc08):2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_dPump_T.dat" u ($1-Tc16):2 w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_dPump_T.dat" u ($1-Tc16):2 w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_dPump_T.dat" u ($1-Tc16):2 w lp lw 3 tit "$L_z=64$"

se k r t
se ou "dFricDensP_Allsize_P_Aligned.tex"
Tc04=2.65343; Tc06=2.55208; Tc08=2.4907; Tc10=2.4907; Tc12=2.4907; Tc14=2.45402; Tc16=2.45402
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_dPump_T.dat" u ($1-Tc04):2 w lp lw 3 tit "$L_z=4$", \
"dat/Lz006Lx0300Ly__Vel10/parallel/res_dPump_T.dat" u ($1-Tc06):2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_dPump_T.dat" u ($1-Tc08):2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz010Lx0500Ly__Vel10/parallel/res_dPump_T.dat" u ($1-Tc10):2 w lp lw 3 tit "$L_z=10$", \
"dat/Lz012Lx0600Ly__Vel10/parallel/res_dPump_T.dat" u ($1-Tc12):2 w lp lw 3 tit "$L_z=12$", \
"dat/Lz014Lx0700Ly__Vel10/parallel/res_dPump_T.dat" u ($1-Tc14):2 w lp lw 3 tit "$L_z=14$", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_dPump_T.dat" u ($1-Tc16):2 w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_dPump_T.dat" u ($1-Tc16):2 w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_dPump_T.dat" u ($1-Tc16):2 w lp lw 3 tit "$L_z=64$"

se k r t
se ou "dFricDensP_Allsize_P_Aligned_.tex"
Tc04=2.65343; Tc06=2.55208; Tc08=2.4907; Tc10=2.4907; Tc12=2.4907; Tc14=2.45402; Tc16=2.45402
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_dPump_T.dat" u ($1-Tc04):2 w lp lw 3 tit "$L_z=4$", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_dPump_T.dat" u ($1-Tc08):2 w lp lw 3 tit "$L_z=6$", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_dPump_T.dat" u ($1-Tc16):2 w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_dPump_T.dat" u ($1-Tc16):2 w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_dPump_T.dat" u ($1-Tc16):2 w lp lw 3 tit "$L_z=64$"

se k r t
se ou "dFricDensP_Allsize_AP_Aligned_HTScaled.tex"
se xr [0:30]
se yr [-0.0005:0.0100]
Tc04=1.35; Tc06=1.65; Tc08=1.85; Tc10=2.05; Tc12=2.07; Tc14=2.21; Tc16=2.25; a=-0.25
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_dPump_T.dat" u (4*($1-Tc04)):(4**a*$2) w lp lw 3 tit "$L_z=4$", \
"dat/Lz006Lx0300Ly__Vel10/antiparallel/res_dPump_T.dat" u (6*($1-Tc06)):(6**a*$2) w lp lw 3 tit "$L_z=6$", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_dPump_T.dat" u (8*($1-Tc08)):(8**a*$2) w lp lw 3 tit "$L_z=6$", \
"dat/Lz010Lx0500Ly__Vel10/antiparallel/res_dPump_T.dat" u (10*($1-Tc10)):(10**a*$2) w lp lw 3 tit "$L_z=10$", \
"dat/Lz012Lx0600Ly__Vel10/antiparallel/res_dPump_T.dat" u (12*($1-Tc12)):(12**a*$2) w lp lw 3 tit "$L_z=12$", \
"dat/Lz014Lx0700Ly__Vel10/antiparallel/res_dPump_T.dat" u (14*($1-Tc14)):(14**a*$2) w lp lw 3 tit "$L_z=14$", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_dPump_T.dat" u (16*($1-Tc16)):(16**a*$2) w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_dPump_T.dat" u (16*($1-Tc16)):(16**a*$2) w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_dPump_T.dat" u (16*($1-Tc16)):(16**a*$2) w lp lw 3 tit "$L_z=64$"

se k r t
se ou "dFricDensP_Allsize_AP_Aligned_HTScaled_.tex"
se xr [0:30]
se yr [-0.0005:0.0100]
Tc04=1.35; Tc06=1.65; Tc08=1.85; Tc10=2.05; Tc12=2.07; Tc14=2.21; Tc16=2.25; a=-0.25
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_dPump_T.dat" u (4*($1-Tc04)):(4**a*$2) w lp lw 3 tit "$L_z=4$", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_dPump_T.dat" u (8*($1-Tc08)):(8**a*$2) w lp lw 3 tit "$L_z=6$", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_dPump_T.dat" u (16*($1-Tc16)):(16**a*$2) w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_dPump_T.dat" u (16*($1-Tc16)):(16**a*$2) w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_dPump_T.dat" u (16*($1-Tc16)):(16**a*$2) w lp lw 3 tit "$L_z=64$"

se k r t
se ou "dFricDensP_Allsize_P_Aligned_HTScaled.tex"
se xr [0:30]
se yr [-0.0005:0.0015]
Tc04=2.65343; Tc06=2.55208; Tc08=2.4907; Tc10=2.4907; Tc12=2.4907; Tc14=2.45402; Tc16=2.45402; a=-1.0
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_dPump_T.dat" u (4*($1-Tc04)):(4**a*$2) w lp lw 3 tit "$L_z=4$", \
"dat/Lz006Lx0300Ly__Vel10/parallel/res_dPump_T.dat" u (6*($1-Tc06)):(6**a*$2) w lp lw 3 tit "$L_z=6$", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_dPump_T.dat" u (8*($1-Tc08)):(8**a*$2) w lp lw 3 tit "$L_z=6$", \
"dat/Lz010Lx0500Ly__Vel10/parallel/res_dPump_T.dat" u (10*($1-Tc10)):(10**a*$2) w lp lw 3 tit "$L_z=10$", \
"dat/Lz012Lx0600Ly__Vel10/parallel/res_dPump_T.dat" u (12*($1-Tc12)):(12**a*$2) w lp lw 3 tit "$L_z=12$", \
"dat/Lz014Lx0700Ly__Vel10/parallel/res_dPump_T.dat" u (14*($1-Tc14)):(14**a*$2) w lp lw 3 tit "$L_z=14$", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_dPump_T.dat" u (16*($1-Tc16)):(16**a*$2) w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_dPump_T.dat" u (16*($1-Tc16)):(16**a*$2) w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_dPump_T.dat" u (16*($1-Tc16)):(16**a*$2) w lp lw 3 tit "$L_z=64$"

se k r t
se ou "dFricDensP_Allsize_P_Aligned_HTScaled_.tex"
se xr [0:30]
se yr [-0.0005:0.0015]
Tc04=2.65343; Tc06=2.55208; Tc08=2.4907; Tc10=2.4907; Tc12=2.4907; Tc14=2.45402; Tc16=2.45402; a=-1.0
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_dPump_T.dat" u (4*($1-Tc04)):(4**a*$2) w lp lw 3 tit "$L_z=4$", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_dPump_T.dat" u (8*($1-Tc08)):(8**a*$2) w lp lw 3 tit "$L_z=6$", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_dPump_T.dat" u (16*($1-Tc16)):(16**a*$2) w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_dPump_T.dat" u (16*($1-Tc16)):(16**a*$2) w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_dPump_T.dat" u (16*($1-Tc16)):(16**a*$2) w lp lw 3 tit "$L_z=64$"

se k r t
se ou "dFricDensP_Allsize_AP_Aligned_LTScaled.tex"
se xr [-3:0]
se yr [-0.0005:0.0100]
Tc04=1.35; Tc06=1.65; Tc08=1.85; Tc10=2.05; Tc12=2.07; Tc14=2.21; Tc16=2.25; a=0.0
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_dPump_T.dat" u (($1-Tc04)):(4**a*$2) w lp lw 3 tit "$L_z=4$", \
"dat/Lz006Lx0300Ly__Vel10/antiparallel/res_dPump_T.dat" u (($1-Tc06)):(6**a*$2) w lp lw 3 tit "$L_z=6$", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_dPump_T.dat" u (($1-Tc08)):(8**a*$2) w lp lw 3 tit "$L_z=6$", \
"dat/Lz010Lx0500Ly__Vel10/antiparallel/res_dPump_T.dat" u (($1-Tc10)):(10**a*$2) w lp lw 3 tit "$L_z=10$", \
"dat/Lz012Lx0600Ly__Vel10/antiparallel/res_dPump_T.dat" u (($1-Tc12)):(12**a*$2) w lp lw 3 tit "$L_z=12$", \
"dat/Lz014Lx0700Ly__Vel10/antiparallel/res_dPump_T.dat" u (($1-Tc14)):(14**a*$2) w lp lw 3 tit "$L_z=14$", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_dPump_T.dat" u (($1-Tc16)):(16**a*$2) w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_dPump_T.dat" u (($1-Tc16)):(16**a*$2) w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_dPump_T.dat" u (($1-Tc16)):(16**a*$2) w lp lw 3 tit "$L_z=64$"

se k r t
se ou "dFricDensP_Allsize_AP_Aligned_LTScaled_.tex"
se xr [-3:0]
se yr [-0.0005:0.0100]
Tc04=1.35; Tc06=1.65; Tc08=1.85; Tc10=2.05; Tc12=2.07; Tc14=2.21; Tc16=2.25; a=0.0
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_dPump_T.dat" u (($1-Tc04)):(4**a*$2) w lp lw 3 tit "$L_z=4$", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_dPump_T.dat" u (($1-Tc08)):(8**a*$2) w lp lw 3 tit "$L_z=6$", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_dPump_T.dat" u (($1-Tc16)):(16**a*$2) w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_dPump_T.dat" u (($1-Tc16)):(16**a*$2) w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_dPump_T.dat" u (($1-Tc16)):(16**a*$2) w lp lw 3 tit "$L_z=64$"

se k r t
se ou "dFricDensP_Allsize_P_Aligned_LTScaled.tex"
se xr [-3:0]
se yr [-0.0005:0.0100]
Tc04=2.65343; Tc06=2.55208; Tc08=2.4907; Tc10=2.4907; Tc12=2.4907; Tc14=2.45402; Tc16=2.45402; a=0.0
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_dPump_T.dat" u (($1-Tc04)):(4**a*$2) w lp lw 3 tit "$L_z=4$", \
"dat/Lz006Lx0300Ly__Vel10/parallel/res_dPump_T.dat" u (($1-Tc06)):(6**a*$2) w lp lw 3 tit "$L_z=6$", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_dPump_T.dat" u (($1-Tc08)):(8**a*$2) w lp lw 3 tit "$L_z=6$", \
"dat/Lz010Lx0500Ly__Vel10/parallel/res_dPump_T.dat" u (($1-Tc10)):(10**a*$2) w lp lw 3 tit "$L_z=10$", \
"dat/Lz012Lx0600Ly__Vel10/parallel/res_dPump_T.dat" u (($1-Tc12)):(12**a*$2) w lp lw 3 tit "$L_z=12$", \
"dat/Lz014Lx0700Ly__Vel10/parallel/res_dPump_T.dat" u (($1-Tc14)):(14**a*$2) w lp lw 3 tit "$L_z=14$", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_dPump_T.dat" u (($1-Tc16)):(16**a*$2) w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_dPump_T.dat" u (($1-Tc16)):(16**a*$2) w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_dPump_T.dat" u (($1-Tc16)):(16**a*$2) w lp lw 3 tit "$L_z=64$"

se k r t
se ou "dFricDensP_Allsize_P_Aligned_LTScaled_.tex"
se xr [-3:0]
se yr [-0.0005:0.0100]
Tc04=2.65343; Tc06=2.55208; Tc08=2.4907; Tc10=2.4907; Tc12=2.4907; Tc14=2.45402; Tc16=2.45402; a=0.0
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_dPump_T.dat" u (($1-Tc04)):(4**a*$2) w lp lw 3 tit "$L_z=4$", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_dPump_T.dat" u (($1-Tc08)):(8**a*$2) w lp lw 3 tit "$L_z=6$", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_dPump_T.dat" u (($1-Tc16)):(16**a*$2) w lp lw 3 tit "$L_z=16$", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_dPump_T.dat" u (($1-Tc16)):(16**a*$2) w lp lw 3 tit "$L_z=32$", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_dPump_T.dat" u (($1-Tc16)):(16**a*$2) w lp lw 3 tit "$L_z=64$"
