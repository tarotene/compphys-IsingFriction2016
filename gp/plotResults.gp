set terminal postscript eps enhanced color

set xtics 1
set mxtics 10
set grid xtics
set grid ytics
set xtics font ",24"   # 目盛りのフォントの変更
set ytics font ",24"   # 目盛りのフォントの変更
set xlabel font ",26" # xlabelのフォントの変更
set ylabel font ",26" # ylabelのフォントの変更

set key box outside below center
set key width -4
set key font ",22"    # 凡例のフォントの変更
set xlabel offset 0,-2
set ylabel offset -2,0
set bmargin 12
set lmargin 17
set key vertical maxrows 3
set key spacing 1.2

# Lz=4
se yr [0:0.006]
se xl "Temperature ({/Times-Italic k}_{/Times-Roman B} {/Times-Italic T/J})"
se yl "{/Times-Italic f}_{/Times-Roman b}(4, {/Times-Italic T})"
se ou "eps/FricDensP_Lz004.eps"
pl \
"dat/Lz004Lx0120Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=120", \
"dat/Lz004Lx0160Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=160", \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=200", \
"dat/Lz004Lx0120Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=120", \
"dat/Lz004Lx0160Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=160", \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=200"

se ou "eps/FricDensD_Lz004.eps"
pl \
"dat/Lz004Lx0120Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2) w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=120", \
"dat/Lz004Lx0160Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2) w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=160", \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2) w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=200", \
"dat/Lz004Lx0120Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2) w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=120", \
"dat/Lz004Lx0160Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2) w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=160", \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2) w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=200"

se yr [-2:0]
se xl "Temperature ({/Times-Italic k}_{/Times-Roman B} {/Times-Italic T/J})"
se yl "{/Symbol-Oblique e}_{/Times-Roman b}(4, {/Times-Italic T})"
se ou "eps/EnDens_Lz004.eps"
pl \
"dat/Lz004Lx0120Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=120", \
"dat/Lz004Lx0160Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=160", \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=200", \
"dat/Lz004Lx0120Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=120", \
"dat/Lz004Lx0160Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=160", \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=200"

# Lz=8
se yr [0:0.006]
se xl "Temperature ({/Times-Italic k}_{/Times-Roman B} {/Times-Italic T/J})"
se yl "{/Times-Italic f}_{/Times-Roman b}(8, {/Times-Italic T})"
se ou "eps/FricDensP_Lz008.eps"
pl \
"dat/Lz008Lx0240Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=240", \
"dat/Lz008Lx0320Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=320", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=400", \
"dat/Lz008Lx0240Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=240", \
"dat/Lz008Lx0320Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=320", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=400"

se ou "eps/FricDensD_Lz008.eps"
pl \
"dat/Lz008Lx0240Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2) w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=240", \
"dat/Lz008Lx0320Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2) w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=320", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_diss_T.dat" u 1:(-$2) w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=400", \
"dat/Lz008Lx0240Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2) w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=240", \
"dat/Lz008Lx0320Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2) w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=320", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_diss_T.dat" u 1:(-$2) w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=400"

se yr [-2:0]
se xl "Temperature ({/Times-Italic k}_{/Times-Roman B} {/Times-Italic T/J})"
se yl "{/Symbol-Oblique e}_{/Times-Roman b}(8, {/Times-Italic T})"
se ou "eps/EnDens_Lz008.eps"
pl \
"dat/Lz008Lx0240Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=240", \
"dat/Lz008Lx0320Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=320", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=400", \
"dat/Lz008Lx0240Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=240", \
"dat/Lz008Lx0320Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=320", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=400"

# Lz=16
se yr [0:0.006]
se xl "Temperature ({/Times-Italic k}_{/Times-Roman B} {/Times-Italic T/J})"
se yl "{/Times-Italic f}_{/Times-Roman b}(16, {/Times-Italic T})"
se ou "eps/FricDensP_Lz016.eps"
pl \
"dat/Lz016Lx0480Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=480", \
"dat/Lz016Lx0640Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=640", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=800", \
"dat/Lz016Lx0480Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=480", \
"dat/Lz016Lx0640Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=640", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=800"

se yr [-2.0:0.0]
se xl "Temperature ({/Times-Italic k}_{/Times-Roman B} {/Times-Italic T/J})"
se yl "{/Symbol-Oblique e}_{/Times-Roman b}(16, {/Times-Italic T})"
se ou "eps/EnDens_Lz016.eps"
pl \
"dat/Lz016Lx0480Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=480", \
"dat/Lz016Lx0640Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=640", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=800", \
"dat/Lz016Lx0480Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=480", \
"dat/Lz016Lx0640Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=640", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=800"

# Lz=32
se yr [0:0.006]
se xl "Temperature ({/Times-Italic k}_{/Times-Roman B} {/Times-Italic T/J})"
se yl "{/Times-Italic f}_{/Times-Roman b}(32, {/Times-Italic T})"
se ou "eps/FricDensP_Lz032.eps"
pl \
"dat/Lz032Lx0960Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=960", \
"dat/Lz032Lx1280Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=1280", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=1600", \
"dat/Lz032Lx0960Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=960", \
"dat/Lz032Lx1280Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=1280", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=1600"

se yr [-2.0:0.0]
se xl "Temperature ({/Times-Italic k}_{/Times-Roman B} {/Times-Italic T/J})"
se yl "{/Symbol-Oblique e}_{/Times-Roman b}(32, {/Times-Italic T})"
se ou "eps/EnDens_Lz032.eps"
pl \
"dat/Lz032Lx0960Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=960", \
"dat/Lz032Lx1280Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=1280", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=1600", \
"dat/Lz032Lx0960Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=060", \
"dat/Lz032Lx1280Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=1280", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=1600"

# Lz=64
se yr [0:0.006]
se xl "Temperature ({/Times-Italic k}_{/Times-Roman B} {/Times-Italic T/J})"
se yl "{/Times-Italic f}_{/Times-Roman b}(64, {/Times-Italic T})"
se ou "eps/FricDensP_Lz064.eps"
pl \
"dat/Lz064Lx1920Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=1920", \
"dat/Lz064Lx2560Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=2560", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=3200", \
"dat/Lz064Lx1920Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=1920", \
"dat/Lz064Lx2560Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=2560", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=3200"

se yr [-2.0:0.0]
se xl "Temperature ({/Times-Italic k}_{/Times-Roman B} {/Times-Italic T/J})"
se yl "{/Symbol-Oblique e}_{/Times-Roman b}(64, {/Times-Italic T})"
se ou "eps/EnDens_Lz064.eps"
pl \
"dat/Lz064Lx1920Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=1920", \
"dat/Lz064Lx2560Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=2560", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Antiparallel, {/Times-Italic L}_{/Times-Italic x}=3200", \
"dat/Lz064Lx1920Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=1920", \
"dat/Lz064Lx2560Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=2560", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 4 tit  "Parallel, {/Times-Italic L}_{/Times-Italic x}=3200"

# se ou "eps/Results_Lz16_sub.eps"
# pl file1 u (1/$1):(($2-$8)/denom3):(($3+$9)/(480*10)) w er notit, \
# file1 u (1/$1):(($2-$8)/denom3) w l tit "Delta F, {/Times-Italic L}_{/Times-Italic x}=480"
#
set key box outside below center
set key width 0
set key font ",24"    # 凡例のフォントの変更
set xlabel offset 0,-1
set ylabel offset -2,0
set bmargin 10
set lmargin 17
set key vertical maxrows 2
set key spacing 1.2

se xl "Temperature ({/Times-Italic k}_{/Times-Roman B} {/Times-Italic T/J})"
se yl "{/Times-Italic f}({/Times-Italic L_z}, {/Times-Italic T})"
se yr [0.000:0.006]
se ou "eps/FricDensP_Allsize_AP.eps"
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=4", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=8", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=16", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=32", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_pump_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=64"

se xl "Temperature ({/Times-Italic k}_{/Times-Roman B} {/Times-Italic T/J})"
se yl "{/Times-Italic f}({/Times-Italic L_z}, {/Times-Italic T})"
se yr [0.000:0.006]
se ou "eps/FricDensP_Allsize_P.eps"
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=4", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=8", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=16", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=32", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_pump_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=64"

se xl "Temperature ({/Times-Italic k}_{/Times-Roman B} {/Times-Italic T/J})"
se yl "{/Symbol-Oblique D}{/Times-Italic f}({/Times-Italic L_z}, {/Times-Italic T})"
se yr [0.000:0.006]
se ou "eps/SubFricDensP_Allsize_.eps"
pl \
"< paste dat/Lz004Lx0200Ly__Vel10/antiparallel/res_pump_T.dat dat/Lz004Lx0200Ly__Vel10/parallel/res_pump_T.dat" using 1:($2-$4) w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=4", \
"< paste dat/Lz008Lx0400Ly__Vel10/antiparallel/res_pump_T.dat dat/Lz008Lx0400Ly__Vel10/parallel/res_pump_T.dat" using 1:($2-$4) w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=8", \
"< paste dat/Lz016Lx0800Ly__Vel10/antiparallel/res_pump_T.dat dat/Lz016Lx0800Ly__Vel10/parallel/res_pump_T.dat" using 1:($2-$4) w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=16", \
"< paste dat/Lz032Lx1600Ly__Vel10/antiparallel/res_pump_T.dat dat/Lz032Lx1600Ly__Vel10/parallel/res_pump_T.dat" using 1:($2-$4) w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=32", \
"< paste dat/Lz064Lx3200Ly__Vel10/antiparallel/res_pump_T.dat dat/Lz064Lx3200Ly__Vel10/parallel/res_pump_T.dat" using 1:($2-$4) w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=64", \

se xl "Temperature ({/Times-Italic k}_{/Times-Roman B} {/Times-Italic T/J})"
se yl "{/Symbol-Oblique e}_{/Times-Roman b}({/Times-Italic L_z}, {/Times-Italic T})"
se ou "eps/EnDens_Allsize_AP.eps"
se yr [-2.0:0.0]
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=4", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=8", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=16", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=32", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_energy_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=64"

se xl "Temperature ({/Times-Italic k}_{/Times-Roman B} {/Times-Italic T/J})"
se yl "{/Symbol-Oblique e}_{/Times-Roman b}({/Times-Italic L_z}, {/Times-Italic T})"
se ou "eps/EnDens_Allsize_P.eps"
se yr [-2.0:0.0]
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=4", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=6", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=16", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=32", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_energy_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=64"

se xl "Temperature ({/Times-Italic k}_{/Times-Roman B} {/Times-Italic T/J})"
se yl "{/Times-Italic c}_{/Times-Roman b}({/Times-Italic L_z}, {/Times-Italic T})"
se ou "eps/dEnDens_Allsize_AP.eps"
se yr [-0.002:2.0]
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=4", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=8", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=16", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=32", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_denergy_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=64"

se xl "Temperature ({/Times-Italic k}_{/Times-Roman B} {/Times-Italic T/J})"
se yl "{/Times-Italic c}_{/Times-Roman b}({/Times-Italic L_z}, {/Times-Italic T})"
se ou "eps/dEnDens_Allsize_P.eps"
se yr [-0.002:2.0]
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=4", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=8", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=16", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=32", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_denergy_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=64"

se yr [-0.002:0.04]
set ytics 0.01

se xr [0.0:5.0]

se xl "Temperature ({/Times-Italic k}_{/Times-Roman B} {/Times-Italic T/J})"
se yl "Temperature Derivative ({/Times-Italic df}/{/Times-Italic dT})"
se ou "eps/dFricDensP_Allsize_AP.eps"
pl \
"dat/Lz004Lx0200Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=4", \
"dat/Lz008Lx0400Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=8", \
"dat/Lz016Lx0800Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=16", \
"dat/Lz032Lx1600Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=32", \
"dat/Lz064Lx3200Ly__Vel10/antiparallel/res_dPump_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=64"

se xl "Temperature ({/Times-Italic k}_{/Times-Roman B} {/Times-Italic T/J})"
se yl "Temperature Derivative ({/Times-Italic df}/{/Times-Italic dT})"
se ou "eps/dFricDensP_Allsize_P.eps"
pl \
"dat/Lz004Lx0200Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=4", \
"dat/Lz008Lx0400Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=8", \
"dat/Lz016Lx0800Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=16", \
"dat/Lz032Lx1600Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=32", \
"dat/Lz064Lx3200Ly__Vel10/parallel/res_dPump_T.dat" u 1:2 w lp lw 4 tit "{/Times-Italic L}_{/Times-Italic z}=64"