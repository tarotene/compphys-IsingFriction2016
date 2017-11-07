se te po eps enh col
se gr
se k r b

vel=10

# Lz=8
#
file1="anl/Lz08Lx80Ly__Vel10/result_beta.dat"
len_x1=80
denom1=len_x1*vel
file2="anl/Lz08Lx160Ly__Vel10/result_beta.dat"
len_x2=160
denom2=len_x2*vel
file3="anl/Lz08Lx240Ly__Vel10/result_beta.dat"
len_x3=240
denom3=len_x3*vel

se ou "Results_Lz08.eps"
pl file1 u (1/$1):($2/denom1):($3/denom1) w er notit, \
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
#
file1="anl/Lz10Lx100Ly__Vel10/result_beta.dat"
len_x1=100
denom1=len_x1*vel
file2="anl/Lz10Lx200Ly__Vel10/result_beta.dat"
len_x2=200
denom2=len_x2*vel
file3="anl/Lz10Lx300Ly__Vel10/result_beta.dat"
len_x3=300
denom3=len_x3*vel

se ou "Results_Lz10.eps"
pl file1 u (1/$1):($2/denom1):($3/denom1) w er notit, \
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
pl file1 u (1/$1):(($2-$8)/denom3):(($3+$9)/(300*10)) w er notit, \
file1 u (1/$1):(($2-$8)/denom3) w l tit "Delta F, L_z=10, L_x=300"

# Lz=12
#
file1="anl/Lz12Lx120Ly__Vel10/result_beta.dat"
len_x1=120
denom1=len_x1*vel
file2="anl/Lz12Lx240Ly__Vel10/result_beta.dat"
len_x2=240
denom2=len_x2*vel
file3="anl/Lz12Lx360Ly__Vel10/result_beta.dat"
len_x3=360
denom3=len_x3*vel

se ou "Results_Lz12.eps"
pl file1 u (1/$1):($2/denom1):($3/denom1) w er notit, \
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
file1="anl/Lz14Lx140Ly__Vel10/result_beta.dat"
len_x1=140
denom1=len_x1*vel
file2="anl/Lz14Lx280Ly__Vel10/result_beta.dat"
len_x2=280
denom2=len_x2*vel
file3="anl/Lz14Lx420Ly__Vel10/result_beta.dat"
len_x3=420
denom3=len_x3*vel

se ou "Results_Lz14.eps"
pl file1 u (1/$1):($2/denom1):($3/denom1) w er notit, \
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
file1="anl/Lz16Lx160Ly__Vel10/result_beta.dat"
len_x1=160
denom1=len_x1*vel
file2="anl/Lz16Lx320Ly__Vel10/result_beta.dat"
len_x2=320
denom2=len_x2*vel
file3="anl/Lz16Lx480Ly__Vel10/result_beta.dat"
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
pl "anl/Lz08Lx240Ly__Vel10/result_beta.dat" u (1/$1):(($2-$8)/240):(($3+$9)/(240*10)) w er notit, \
"anl/Lz08Lx240Ly__Vel10/result_beta.dat" u (1/$1):(($2-$8)/240) w l tit "Delta F, L_z=8, L_x=240", \
"anl/Lz10Lx300Ly__Vel10/result_beta.dat" u (1/$1):(($2-$8)/300):(($3+$9)/(300*10)) w er notit, \
"anl/Lz10Lx300Ly__Vel10/result_beta.dat" u (1/$1):(($2-$8)/300) w l tit "Delta F, L_z=10, L_x=300", \
"anl/Lz12Lx360Ly__Vel10/result_beta.dat" u (1/$1):(($2-$8)/360):(($3+$9)/(360*10)) w er notit, \
"anl/Lz12Lx360Ly__Vel10/result_beta.dat" u (1/$1):(($2-$8)/360) w l tit "Delta F, L_z=12, L_x=360", \
"anl/Lz14Lx420Ly__Vel10/result_beta.dat" u (1/$1):(($2-$8)/420):(($3+$9)/(420*10)) w er notit, \
"anl/Lz14Lx420Ly__Vel10/result_beta.dat" u (1/$1):(($2-$8)/420) w l tit "Delta F, L_z=14, L_x=420", \
"anl/Lz16Lx480Ly__Vel10/result_beta.dat" u (1/$1):(($2-$8)/480):(($3+$9)/(480*10)) w er notit, \
"anl/Lz16Lx480Ly__Vel10/result_beta.dat" u (1/$1):(($2-$8)/480) w l tit "Delta F, L_z=16, L_x=480"

se ou "Results_Allsize_sub_ScaledLowTemp.eps"
se xr [-20:0]
se yr [0:0.04]
a=1/4
pl "anl/Lz08Lx240Ly__Vel10/result_beta.dat" u (((1/$1)-2.3)*8):((($2-$8)/240)*8**a) w lp tit "Delta F, L_z=8, L_x=240", \
"anl/Lz10Lx300Ly__Vel10/result_beta.dat" u (((1/$1)-2.3)*10):((($2-$8)/300)*10**a) w lp tit "Delta F, L_z=10, L_x=300", \
"anl/Lz12Lx360Ly__Vel10/result_beta.dat" u (((1/$1)-2.3)*12):((($2-$8)/360)*12**a) w lp tit "Delta F, L_z=12, L_x=360", \
"anl/Lz14Lx420Ly__Vel10/result_beta.dat" u (((1/$1)-2.3)*14):((($2-$8)/420)*14**a) w lp tit "Delta F, L_z=14, L_x=420", \
"anl/Lz16Lx480Ly__Vel10/result_beta.dat" u (((1/$1)-2.3)*16):((($2-$8)/480)*16**a) w lp tit "Delta F, L_z=16, L_x=480"
se ou "Results_Allsize_sub_ScaledHighTemp.eps"
se xr [0:10]
se yr [0:0.04]
a=1/4
pl "anl/Lz08Lx240Ly__Vel10/result_beta.dat" u (((1/$1)-2.3)*8):((($2-$8)/240)*8**a) w lp tit "Delta F, L_z=8, L_x=240", \
"anl/Lz10Lx300Ly__Vel10/result_beta.dat" u (((1/$1)-2.3)*10):((($2-$8)/300)*10**a) w lp tit "Delta F, L_z=10, L_x=300", \
"anl/Lz12Lx360Ly__Vel10/result_beta.dat" u (((1/$1)-2.3)*12):((($2-$8)/360)*12**a) w lp tit "Delta F, L_z=12, L_x=360", \
"anl/Lz14Lx420Ly__Vel10/result_beta.dat" u (((1/$1)-2.3)*14):((($2-$8)/420)*14**a) w lp tit "Delta F, L_z=14, L_x=420", \
"anl/Lz16Lx480Ly__Vel10/result_beta.dat" u (((1/$1)-2.3)*16):((($2-$8)/480)*16**a) w lp tit "Delta F, L_z=16, L_x=480"
