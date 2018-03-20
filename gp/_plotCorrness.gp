set terminal postscript eps color

hl_t = l_t / 2
do for [s in set_s] {
  infile1 = "beta".beta."/ac_en_bulk_s".s.".bin"
  infile2 = "beta".beta."/ac_en_edge_s".s.".bin"
  infile3 = "beta".beta."/ac_en_bulk_s".s.".bin"
  infile4 = "beta".beta."/ac_en_edge_s".s.".bin"
  outfile1 = "ac_eb_s".s."_beta".beta.".eps"
  outfile2 = "ac_ee_s".s."_beta".beta.".eps"
  outfile3 = "ac_mb_s".s."_beta".beta.".eps"
  outfile4 = "ac_me_s".s."_beta".beta.".eps"

  set xrange [0:2000]
  set grid

  set output outfile1
  plot  infile1 binary array=2000 format="%float" smooth unique notitle
  set output outfile2
  plot  infile2 binary array=2000 format="%float" smooth unique notitle
  set output outfile3
  plot  infile3 binary array=2000 format="%float" smooth unique notitle
  set output outfile4
  plot  infile4 binary array=2000 format="%float" smooth unique notitle
}
