se te po eps enh col
se gr
se k r b

# set tics font ",24"   # 目盛りのフォントの変更
# set xlabel font ",32" # xlabelのフォントの変更
# set ylabel font ",32" # ylabelのフォントの変更
# set zlabel font ",32" # zlabelのフォントの変更
# set key font ",40"    # 凡例のフォントの変更

vel=10

# Lz=4

file1="dat/Lz04Lx40Ly__Vel10/result_beta.dat"
len_x1=40
denom1=len_x1*vel
file2="dat/Lz04Lx80Ly__Vel10/result_beta.dat"
len_x2=80
denom2=len_x2*vel
file3="dat/Lz04Lx120Ly__Vel10/result_beta.dat"
len_x3=120
denom3=len_x3*vel

se ou "Results_Lz04.eps"
pl file1 u (1/$1):($2/denom1):($3/denom1) w er notit, \
file1 u (1/$1):($2/denom1) w l tit "Antipar, L_z=4, L_x=40", \
file1 u (1/$1):($8/denom1):($9/denom1) w er notit, \
file1 u (1/$1):($8/denom1) w l tit "Par, L_z=4, L_x=40", \
file2 u (1/$1):($2/denom2):($3/denom2) w er notit, \
file2 u (1/$1):($2/denom2) w l tit "Antipar, L_z=4, L_x=80", \
file2 u (1/$1):($8/denom2):($9/denom2) w er notit, \
file2 u (1/$1):($8/denom2) w l tit "Par, L_z=4, L_x=80", \
file3 u (1/$1):($2/denom3):($3/denom3) w er notit, \
file3 u (1/$1):($2/denom3) w l tit "Antipar, L_z=4, L_x=120", \
file3 u (1/$1):($8/denom3):($9/denom3) w er notit, \
file3 u (1/$1):($8/denom3) w l tit "Par, L_z=4, L_x=120"

se ou "Results_Lz04_sub.eps"
pl file1 u (1/$1):(($2-$8)/denom3):(($3+$9)/(120*10)) w er notit, \
file1 u (1/$1):(($2-$8)/denom3) w l tit "Delta F, L_z=4, L_x=120"

# Lz=6

file1="dat/Lz06Lx60Ly__Vel10/result_beta.dat"
len_x1=60
denom1=len_x1*vel
file2="dat/Lz06Lx120Ly__Vel10/result_beta.dat"
len_x2=120
denom2=len_x2*vel
file3="dat/Lz06Lx180Ly__Vel10/result_beta.dat"
len_x3=180
denom3=len_x3*vel

se ou "Results_Lz06.eps"
pl file1 u (1/$1):($2/denom1):($3/denom1) w er notit, \
file1 u (1/$1):($2/denom1) w l tit "Antipar, L_z=6, L_x=60", \
file1 u (1/$1):($8/denom1):($9/denom1) w er notit, \
file1 u (1/$1):($8/denom1) w l tit "Par, L_z=6, L_x=80", \
file2 u (1/$1):($2/denom2):($3/denom2) w er notit, \
file2 u (1/$1):($2/denom2) w l tit "Antipar, L_z=6, L_x=120", \
file2 u (1/$1):($8/denom2):($9/denom2) w er notit, \
file2 u (1/$1):($8/denom2) w l tit "Par, L_z=6, L_x=160", \
file3 u (1/$1):($2/denom3):($3/denom3) w er notit, \
file3 u (1/$1):($2/denom3) w l tit "Antipar, L_z=6, L_x=180", \
file3 u (1/$1):($8/denom3):($9/denom3) w er notit, \
file3 u (1/$1):($8/denom3) w l tit "Par, L_z=6, L_x=240"

se ou "Results_Lz06_sub.eps"
pl file1 u (1/$1):(($2-$8)/denom3):(($3+$9)/(180*10)) w er notit, \
file1 u (1/$1):(($2-$8)/denom3) w l tit "Delta F, L_z=6, L_x=180"

# Lz=8

file1="dat/Lz08Lx80Ly__Vel10/result_beta.dat"
len_x1=80
denom1=len_x1*vel
file2="dat/Lz08Lx160Ly__Vel10/result_beta.dat"
len_x2=160
denom2=len_x2*vel
file3="dat/Lz08Lx240Ly__Vel10/result_beta.dat"
len_x3=240
denom3=len_x3*vel

se ou "Results_Lz08.eps"
pl \
file1 u (1/$1):($2/denom1):($3/denom1) w er notit, \
file1 u (1/$1):($2/denom1) w l tit "Antipar, L_z=8, L_x=80", \
file1 u (1/$1):($8/denom1):($9/denom1) w er notit, \
file1 u (1/$1):($8/denom1) w l tit "Par, L_z=8, L_x=80", \
file2 u (1/$1):($2/denom2):($3/denom2) w er notit, \
file2 u (1/$1):($2/denom2) w l tit "Antipar, L_z=8, L_x=160", \
file2 u (1/$1):($8/denom2):($9/denom2) w er notit, \
file2 u (1/$1):($8/denom2) w l tit "Par, L_z=8, L_x=160", \
file3 u (1/$1):($2/denom3):($3/denom3) w er notit, \
file3 u (1/$1):($2/denom3) w l tit "Antipar, L_z=8, L_x=240", \
file3 u (1/$1):($8/denom3):($9/denom3) w er notit, \
file3 u (1/$1):($8/denom3) w l tit "Par, L_z=8, L_x=240"

se ou "Results_Lz08_sub.eps"
pl file1 u (1/$1):(($2-$8)/denom3):(($3+$9)/(240*10)) w er notit, \
file1 u (1/$1):(($2-$8)/denom3) w l tit "Delta F, L_z=8, L_x=240"

# Lz=10

file1="dat/Lz10Lx100Ly__Vel10/result_beta.dat"
len_x1=100
denom1=len_x1*vel
file2="dat/Lz10Lx200Ly__Vel10/result_beta.dat"
len_x2=200
denom2=len_x2*vel
file3="dat/Lz10Lx300Ly__Vel10/result_beta.dat"
len_x3=300
denom3=len_x3*vel

se ou "Results_Lz10.eps"
pl \
file1 u (1/$1):($2/denom1):($3/denom1) w er notit, \
file1 u (1/$1):($2/denom1) w l tit "Antipar, L_z=10, L_x=100", \
file1 u (1/$1):($8/denom1):($9/denom1) w er notit, \
file1 u (1/$1):($8/denom1) w l tit "Par, L_z=10, L_x=100", \
file2 u (1/$1):($2/denom2):($3/denom2) w er notit, \
file2 u (1/$1):($2/denom2) w l tit "Antipar, L_z=10, L_x=200", \
file2 u (1/$1):($8/denom2):($9/denom2) w er notit, \
file2 u (1/$1):($8/denom2) w l tit "Par, L_z=10, L_x=200", \
file3 u (1/$1):($2/denom3):($3/denom3) w er notit, \
file3 u (1/$1):($2/denom3) w l tit "Antipar, L_z=10, L_x=300", \
file3 u (1/$1):($8/denom3):($9/denom3) w er notit, \
file3 u (1/$1):($8/denom3) w l tit "Par, L_z=10, L_x=300"

se ou "Results_Lz10_sub.eps"
pl \
file1 u (1/$1):(($2-$8)/denom3):(($3+$9)/(300*10)) w er notit, \
file1 u (1/$1):(($2-$8)/denom3) w l tit "Delta F, L_z=10, L_x=300"

# Lz=12
#
file1="dat/Lz12Lx120Ly__Vel10/result_beta.dat"
len_x1=120
denom1=len_x1*vel
file2="dat/Lz12Lx240Ly__Vel10/result_beta.dat"
len_x2=240
denom2=len_x2*vel
file3="dat/Lz12Lx360Ly__Vel10/result_beta.dat"
len_x3=360
denom3=len_x3*vel

se ou "Results_Lz12.eps"
pl \
file1 u (1/$1):($2/denom1):($3/denom1) w er notit, \
file1 u (1/$1):($2/denom1) w l tit "Antipar, L_z=12, L_x=120", \
file1 u (1/$1):($8/denom1):($9/denom1) w er notit, \
file1 u (1/$1):($8/denom1) w l tit "Par, L_z=12, L_x=120", \
file2 u (1/$1):($2/denom2):($3/denom2) w er notit, \
file2 u (1/$1):($2/denom2) w l tit "Antipar, L_z=12, L_x=240", \
file2 u (1/$1):($8/denom2):($9/denom2) w er notit, \
file2 u (1/$1):($8/denom2) w l tit "Par, L_z=12, L_x=240", \
file3 u (1/$1):($2/denom3):($3/denom3) w er notit, \
file3 u (1/$1):($2/denom3) w l tit "Antipar, L_z=12, L_x=360", \
file3 u (1/$1):($8/denom3):($9/denom3) w er notit, \
file3 u (1/$1):($8/denom3) w l tit "Par, L_z=12, L_x=360"

se ou "Results_Lz12_sub.eps"
pl file1 u (1/$1):(($2-$8)/denom3):(($3+$9)/(360*10)) w er notit, \
file1 u (1/$1):(($2-$8)/denom3) w l tit "Delta F, L_z=12, L_x=360"

# Lz=14
#
file1="dat/Lz14Lx140Ly__Vel10/result_beta.dat"
len_x1=140
denom1=len_x1*vel
file2="dat/Lz14Lx280Ly__Vel10/result_beta.dat"
len_x2=280
denom2=len_x2*vel
file3="dat/Lz14Lx420Ly__Vel10/result_beta.dat"
len_x3=420
denom3=len_x3*vel

se ou "Results_Lz14.eps"
pl \
file1 u (1/$1):($2/denom1):($3/denom1) w er notit, \
file1 u (1/$1):($2/denom1) w l tit "Antipar, L_z=14, L_x=140", \
file1 u (1/$1):($8/denom1):($9/denom1) w er notit, \
file1 u (1/$1):($8/denom1) w l tit "Par, L_z=14, L_x=140", \
file2 u (1/$1):($2/denom2):($3/denom2) w er notit, \
file2 u (1/$1):($2/denom2) w l tit "Antipar, L_z=14, L_x=280", \
file2 u (1/$1):($8/denom2):($9/denom2) w er notit, \
file2 u (1/$1):($8/denom2) w l tit "Par, L_z=14, L_x=280", \
file3 u (1/$1):($2/denom3):($3/denom3) w er notit, \
file3 u (1/$1):($2/denom3) w l tit "Antipar, L_z=14, L_x=420", \
file3 u (1/$1):($8/denom3):($9/denom3) w er notit, \
file3 u (1/$1):($8/denom3) w l tit "Par, L_z=14, L_x=420"

se ou "Results_Lz14_sub.eps"
pl file1 u (1/$1):(($2-$8)/denom3):(($3+$9)/(420*10)) w er notit, \
file1 u (1/$1):(($2-$8)/denom3) w l tit "Delta F, L_z=14, L_x=420"

# Lz=16
#
file1="dat/Lz16Lx160Ly__Vel10/result_beta.dat"
len_x1=160
denom1=len_x1*vel
file2="dat/Lz16Lx320Ly__Vel10/result_beta.dat"
len_x2=320
denom2=len_x2*vel
file3="dat/Lz16Lx480Ly__Vel10/result_beta.dat"
len_x3=480
denom3=len_x3*vel

se ou "Results_Lz16.eps"
pl file1 u (1/$1):($2/denom1):($3/denom1) w er notit, \
file1 u (1/$1):($2/denom1) w l tit "Antipar, L_z=16, L_x=160", \
file1 u (1/$1):($8/denom1):($9/denom1) w er notit, \
file1 u (1/$1):($8/denom1) w l tit "Par, L_z=16, L_x=160", \
file2 u (1/$1):($2/denom2):($3/denom2) w er notit, \
file2 u (1/$1):($2/denom2) w l tit "Antipar, L_z=16, L_x=320", \
file2 u (1/$1):($8/denom2):($9/denom2) w er notit, \
file2 u (1/$1):($8/denom2) w l tit "Par, L_z=16, L_x=320", \
file3 u (1/$1):($2/denom3):($3/denom3) w er notit, \
file3 u (1/$1):($2/denom3) w l tit "Antipar, L_z=16, L_x=480", \
file3 u (1/$1):($8/denom3):($9/denom3) w er notit, \
file3 u (1/$1):($8/denom3) w l tit "Par, L_z=16, L_x=480"

se ou "Results_Lz16_sub.eps"
pl file1 u (1/$1):(($2-$8)/denom3):(($3+$9)/(480*10)) w er notit, \
file1 u (1/$1):(($2-$8)/denom3) w l tit "Delta F, L_z=16, L_x=480"

se k r t
se ou "Results_Allsize_sub.eps"
se yr [0:0.05]
pl \
"dat/Lz04Lx120Ly__Vel10/result_beta.dat" u (1/$1):(($2-$8)/120):(($3+$9)/(120*10)) w er ps 1 notit, \
"dat/Lz04Lx120Ly__Vel10/result_beta.dat" u (1/$1):(($2-$8)/120) w l lw 3 tit "L_z=4", \
"dat/Lz06Lx180Ly__Vel10/result_beta.dat" u (1/$1):(($2-$8)/180):(($3+$9)/(180*10)) w er ps 1 notit, \
"dat/Lz06Lx180Ly__Vel10/result_beta.dat" u (1/$1):(($2-$8)/180) w l lw 3 tit "L_z=6", \
"dat/Lz08Lx240Ly__Vel10/result_beta.dat" u (1/$1):(($2-$8)/240):(($3+$9)/(240*10)) w er ps 1 notit, \
"dat/Lz08Lx240Ly__Vel10/result_beta.dat" u (1/$1):(($2-$8)/240) w l lw 3 tit "L_z=8", \
"dat/Lz10Lx300Ly__Vel10/result_beta.dat" u (1/$1):(($2-$8)/300):(($3+$9)/(300*10)) w er ps 1 notit, \
"dat/Lz10Lx300Ly__Vel10/result_beta.dat" u (1/$1):(($2-$8)/300) w l lw 3 tit "L_z=10", \
"dat/Lz12Lx360Ly__Vel10/result_beta.dat" u (1/$1):(($2-$8)/360):(($3+$9)/(360*10)) w er ps 1 notit, \
"dat/Lz12Lx360Ly__Vel10/result_beta.dat" u (1/$1):(($2-$8)/360) w l lw 3 tit "L_z=12", \
"dat/Lz14Lx420Ly__Vel10/result_beta.dat" u (1/$1):(($2-$8)/420):(($3+$9)/(420*10)) w er ps 1 notit, \
"dat/Lz14Lx420Ly__Vel10/result_beta.dat" u (1/$1):(($2-$8)/420) w l lw 3 tit "L_z=14", \
"dat/Lz16Lx480Ly__Vel10/result_beta.dat" u (1/$1):(($2-$8)/480):(($3+$9)/(480*10)) w er ps 1 notit, \
"dat/Lz16Lx480Ly__Vel10/result_beta.dat" u (1/$1):(($2-$8)/480) w l lw 3 tit "L_z=16"

se ou "Results_Allsize_sub_Aligned.eps"
Tc04=2.02
Tc06=2.06
Tc08=2.14
Tc10=2.18
Tc12=2.26
Tc14=2.30
Tc16=2.32
pl \
"dat/Lz04Lx120Ly__Vel10/result_beta.dat" u ((1/$1)-Tc04):(($2-$8)/120):(($3+$9)/(120*10)) w er ps 1 notit, \
"dat/Lz04Lx120Ly__Vel10/result_beta.dat" u ((1/$1)-Tc04):(($2-$8)/120) w l lw 3 tit "L_z=4", \
"dat/Lz06Lx180Ly__Vel10/result_beta.dat" u ((1/$1)-Tc06):(($2-$8)/180):(($3+$9)/(180*10)) w er ps 1 notit, \
"dat/Lz06Lx180Ly__Vel10/result_beta.dat" u ((1/$1)-Tc06):(($2-$8)/180) w l lw 3 tit "L_z=6", \
"dat/Lz08Lx240Ly__Vel10/result_beta.dat" u ((1/$1)-Tc08):(($2-$8)/240):(($3+$9)/(240*10)) w er ps 1 notit, \
"dat/Lz08Lx240Ly__Vel10/result_beta.dat" u ((1/$1)-Tc08):(($2-$8)/240) w l lw 3 tit "L_z=8", \
"dat/Lz10Lx300Ly__Vel10/result_beta.dat" u ((1/$1)-Tc10):(($2-$8)/300):(($3+$9)/(300*10)) w er ps 1 notit, \
"dat/Lz10Lx300Ly__Vel10/result_beta.dat" u ((1/$1)-Tc10):(($2-$8)/300) w l lw 3 tit "L_z=10", \
"dat/Lz12Lx360Ly__Vel10/result_beta.dat" u ((1/$1)-Tc12):(($2-$8)/360):(($3+$9)/(360*10)) w er ps 1 notit, \
"dat/Lz12Lx360Ly__Vel10/result_beta.dat" u ((1/$1)-Tc12):(($2-$8)/360) w l lw 3 tit "L_z=12", \
"dat/Lz14Lx420Ly__Vel10/result_beta.dat" u ((1/$1)-Tc14):(($2-$8)/420):(($3+$9)/(420*10)) w er ps 1 notit, \
"dat/Lz14Lx420Ly__Vel10/result_beta.dat" u ((1/$1)-Tc14):(($2-$8)/420) w l lw 3 tit "L_z=14", \
"dat/Lz16Lx480Ly__Vel10/result_beta.dat" u ((1/$1)-Tc16):(($2-$8)/480):(($3+$9)/(480*10)) w er ps 1 notit, \
"dat/Lz16Lx480Ly__Vel10/result_beta.dat" u ((1/$1)-Tc16):(($2-$8)/480) w l lw 3 tit "L_z=16"

se ou "Results_Allsize_sub_Scaled_LT.eps"
set autoscale
set logscale
pl \
"dat/Lz04Lx120Ly__Vel10/result_beta.dat" u (((1/$1))):((($2-$8)/120)*(4**(0.25))) w lp tit "Delta F, L_z=4, L_x=120", \
"dat/Lz06Lx180Ly__Vel10/result_beta.dat" u (((1/$1))):((($2-$8)/180)*(6**(0.25))) w lp tit "Delta F, L_z=6, L_x=180", \
"dat/Lz08Lx240Ly__Vel10/result_beta.dat" u (((1/$1))):((($2-$8)/240)*(8**(0.25))) w lp tit "Delta F, L_z=8, L_x=240", \
"dat/Lz10Lx300Ly__Vel10/result_beta.dat" u (((1/$1))):((($2-$8)/300)*(10**(0.25))) w lp tit "Delta F, L_z=10, L_x=300", \
"dat/Lz12Lx360Ly__Vel10/result_beta.dat" u (((1/$1))):((($2-$8)/360)*(12**(0.25))) w lp tit "Delta F, L_z=12, L_x=360", \
"dat/Lz14Lx420Ly__Vel10/result_beta.dat" u (((1/$1))):((($2-$8)/420)*(14**(0.25))) w lp tit "Delta F, L_z=14, L_x=420", \
"dat/Lz16Lx480Ly__Vel10/result_beta.dat" u (((1/$1))):((($2-$8)/480)*(16**(0.25))) w lp tit "Delta F, L_z=16, L_x=480"

se ou "Results_Allsize_sub_Scaled_HT.eps"
set autoscale
set logscale
pl \
"dat/Lz04Lx120Ly__Vel10/result_beta.dat" u 1:((($2-$8)/120)*(4**(0.25))) w lp tit "Delta F, L_z=4, L_x=120", \
"dat/Lz06Lx180Ly__Vel10/result_beta.dat" u 1:((($2-$8)/180)*(6**(0.25))) w lp tit "Delta F, L_z=6, L_x=180", \
"dat/Lz08Lx240Ly__Vel10/result_beta.dat" u 1:((($2-$8)/240)*(8**(0.25))) w lp tit "Delta F, L_z=8, L_x=240", \
"dat/Lz10Lx300Ly__Vel10/result_beta.dat" u 1:((($2-$8)/300)*(10**(0.25))) w lp tit "Delta F, L_z=10, L_x=300", \
"dat/Lz12Lx360Ly__Vel10/result_beta.dat" u 1:((($2-$8)/360)*(12**(0.25))) w lp tit "Delta F, L_z=12, L_x=360", \
"dat/Lz14Lx420Ly__Vel10/result_beta.dat" u 1:((($2-$8)/420)*(14**(0.25))) w lp tit "Delta F, L_z=14, L_x=420", \
"dat/Lz16Lx480Ly__Vel10/result_beta.dat" u 1:((($2-$8)/480)*(16**(0.25))) w lp tit "Delta F, L_z=16, L_x=480"

se ou "Results_Allsize_sub_Aligned_Scaled.eps"
set autoscale
set logscale
pl \
"dat/Lz04Lx120Ly__Vel10/result_beta.dat" u (((1/$1)-Tc04)*4):((($2-$8)/120)*(4**(0.25))) w lp tit "Delta F, L_z=4, L_x=120", \
"dat/Lz06Lx180Ly__Vel10/result_beta.dat" u (((1/$1)-Tc06)*6):((($2-$8)/180)*(6**(0.25))) w lp tit "Delta F, L_z=6, L_x=180", \
"dat/Lz08Lx240Ly__Vel10/result_beta.dat" u (((1/$1)-Tc08)*8):((($2-$8)/240)*(8**(0.25))) w lp tit "Delta F, L_z=8, L_x=240", \
"dat/Lz10Lx300Ly__Vel10/result_beta.dat" u (((1/$1)-Tc10)*10):((($2-$8)/300)*(10**(0.25))) w lp tit "Delta F, L_z=10, L_x=300", \
"dat/Lz12Lx360Ly__Vel10/result_beta.dat" u (((1/$1)-Tc12)*12):((($2-$8)/360)*(12**(0.25))) w lp tit "Delta F, L_z=12, L_x=360", \
"dat/Lz14Lx420Ly__Vel10/result_beta.dat" u (((1/$1)-Tc14)*14):((($2-$8)/420)*(14**(0.25))) w lp tit "Delta F, L_z=14, L_x=420", \
"dat/Lz16Lx480Ly__Vel10/result_beta.dat" u (((1/$1)-Tc16)*16):((($2-$8)/480)*(16**(0.25))) w lp tit "Delta F, L_z=16, L_x=480"
