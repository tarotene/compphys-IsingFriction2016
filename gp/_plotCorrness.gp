set xrange [0:10000]
set yrange [-1:1]
set grid

set multiplot layout 5,10
do for [beta in set_beta] {
  infile = "beta".beta."/ac_en_bulk_s".s.".bin"
  plot  infile binary array=10000 format="%float" smooth unique title "beta = ".beta
  }
unset multiplot

if (exist("TERM") == 0 || TERM eq "Default") \
  pause -1 "click ON to make PS file"; \
  TERM = "PS"; \
  set out outfile1 = "ac_eb_s".s.".eps"; \
  set term postscript

  set xrange [0:10000]
  set yrange [-1:1]
  set grid

  set terminal postscript color enhanced portrait size 7in, 10in; \
  reread
  
  TERM = "Default"
set outset term pop

set multiplot layout 5,10
do for [beta in set_beta] {
  infile = "beta".beta."/ac_en_edge_s".s.".bin"
  plot  infile binary array=10000 format="%float" smooth unique title "beta = ".beta
  }
unset multiplot

if (exist("TERM") == 0 || TERM eq "Default") \
  pause -1 "click ON to make PS file"; \
  TERM = "PS"; \
  set out outfile1 = "ac_ee_s".s.".eps"; \
  set term postscript

  set xrange [0:10000]
  set yrange [-1:1]
  set grid

  set terminal postscript color enhanced portrait size 7in, 10in; \
  reread
  
  TERM = "Default"
set outset term pop

set multiplot layout 5,10
do for [beta in set_beta] {
  infile = "beta".beta."/ac_m_bulk_s".s.".bin"
  plot  infile binary array=10000 format="%float" smooth unique title "beta = ".beta
  }
unset multiplot

if (exist("TERM") == 0 || TERM eq "Default") \
  pause -1 "click ON to make PS file"; \
  TERM = "PS"; \
  set out outfile1 = "ac_mb_s".s.".eps"; \
  set term postscript

  set xrange [0:10000]
  set yrange [-1:1]
  set grid

  set terminal postscript color enhanced portrait size 7in, 10in; \
  reread
  
  TERM = "Default"
set outset term pop

set multiplot layout 5,10
do for [beta in set_beta] {
  infile = "beta".beta."/ac_m_edge_s".s.".bin"
  plot  infile binary array=10000 format="%float" smooth unique title "beta = ".beta
  }
unset multiplot

if (exist("TERM") == 0 || TERM eq "Default") \
  pause -1 "click ON to make PS file"; \
  TERM = "PS"; \
  set out outfile1 = "ac_me_s".s.".eps"; \
  set term postscript

  set xrange [0:10000]
  set yrange [-1:1]
  set grid

  set terminal postscript color enhanced portrait size 7in, 10in; \
  reread
  
  TERM = "Default"
set outset term pop
