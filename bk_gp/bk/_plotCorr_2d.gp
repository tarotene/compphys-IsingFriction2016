set terminal postscript eps color enhanced
set grid

set output "eps_s".s."/physquan_beta".beta.".eps"

set multiplot
set lmargin screen 0.1; set rmargin screen 0.5
set tmargin screen 0.9; set bmargin screen 0.74
set xrange [0:200]
set xtics 40
set yrange [-2*l_x*l_z:2*l_x*l_z]
set format x ""
plot "beta".beta."/eb_sweep/en_bulk_s".s."_sweep.bin" binary array=20000 format="%int%*99int" smooth unique lw 4 title "energy (bulk)"

set tmargin screen 0.74; set bmargin screen 0.58
set yrange [-2*l_x:2*l_x]
plot "beta".beta."/ee_sweep/en_edge_s".s."_sweep.bin" binary array=20000 format="%int%*99int" smooth unique lw 4 title "energy (edge)"

set tmargin screen 0.58; set bmargin screen 0.42
set yrange [-l_x*l_z:l_x*l_z]
plot "beta".beta."/mb_sweep/m_bulk_s".s."_sweep.bin" binary array=20000 format="%int%*99int" smooth unique lw 4 title "magnetization (bulk)"

set tmargin screen 0.42; set bmargin screen 0.26
set yrange [-2*l_x:2*l_x]
plot "beta".beta."/me_sweep/m_edge_s".s."_sweep.bin" binary array=20000 format="%int%*99int" smooth unique lw 4 title "magnetization (edge)"

set tmargin screen 0.26; set bmargin screen 0.1
set yrange [-l_x:l_x]
set format x
plot "beta".beta."/p_sweep/pump_s".s."_sweep.bin" binary array=20000 format="%int%*99int" smooth unique lw 4 title "pumping"

set lmargin screen 0.5; set rmargin screen 0.9
set tmargin screen 0.9; set bmargin screen 0.74
set xrange [0:100]
set xtics 20
set yrange [-1:1]
set y2tics
set format y ""
set format y2
set format x ""
plot "beta".beta."/ac_en_bulk_s".s.".bin" binary array=10000 format="%float%*9float" smooth unique lw 4 title "ac: energy (bulk)"

set tmargin screen 0.74; set bmargin screen 0.58
plot "beta".beta."/ac_en_edge_s".s.".bin" binary array=10000 format="%float%*9float" smooth unique lw 4 title "ac: energy (edge)"

set tmargin screen 0.58; set bmargin screen 0.42
plot "beta".beta."/ac_m_bulk_s".s.".bin" binary array=10000 format="%float%*9float" smooth unique lw 4 title "ac: magnetization (bulk)"

set tmargin screen 0.42; set bmargin screen 0.26
plot "beta".beta."/ac_m_edge_s".s.".bin" binary array=10000 format="%float%*9float" smooth unique lw 4 title "ac: magnetization (edge)"

set tmargin screen 0.26; set bmargin screen 0.1
set format x
plot "beta".beta."/ac_p_s".s.".bin" binary array=10000 format="%float%*9float" smooth unique lw 4 title "ac: pumping"
unset multiplot