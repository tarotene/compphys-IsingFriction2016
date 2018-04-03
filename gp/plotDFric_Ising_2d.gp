set terminal postscript eps enhanced color
set encoding utf8
set grid

# params
    a=2.0
    b=9
    T00=2.2
    T01=2.1
    T02=2.2
    T03=2.2
    T04=2.2
    T05=2.2
    T06=2.2
    T10=2.0
    T11=2.1
    T12=2.1
    T13=2.1
    T14=2.1
    T15=2.1
    T16=2.1
    T20=2.1
    T21=2.1
    T22=2.2
    T23=2.2
    T24=2.2
    T25=2.2
    T26=2.2
    T30=2.1
    T31=2.2
    T32=2.4
    T33=2.4
    T34=2.4
    T35=2.4
    T36=2.4
    T40=2.3
    T41=2.3
    T42=2.4
    T43=2.4
    T44=2.4
    T45=2.4
    T46=2.4

set xr [-2.5:2.5]
set yr [0.0:0.6]

# vscaling
    set output "eps/vscaled_dfric_Lz0002.eps"
        pl \
        "res/Ising_2d/Lz0002Lx0064Ly__Vel0008/delta_phys.dat" u (1/$2-T02):(exp(2/$3)*$12) w lp lc rgb '#377EB8',\
        "res/Ising_2d/Lz0002Lx0064Ly__Vel0016/delta_phys.dat" u (1/$2-T03):(exp(2/$3)*$12) w lp lc rgb '#E41A1C',\
        "res/Ising_2d/Lz0002Lx0064Ly__Vel0032/delta_phys.dat" u (1/$2-T04):(exp(2/$3)*$12) w lp lc rgb '#4DAF4A',\
        "res/Ising_2d/Lz0002Lx0064Ly__Vel0064/delta_phys.dat" u (1/$2-T05):(exp(2/$3)*$12) w lp lc rgb '#984EA3',\
        "res/Ising_2d/Lz0002Lx0064Ly__Vel0128/delta_phys.dat" u (1/$2-T06):(exp(2/$3)*$12) w lp lc rgb '#FF7F00'

    set output "eps/vscaled_dfric_Lz0004.eps"
        pl \
        "res/Ising_2d/Lz0004Lx0128Ly__Vel0008/delta_phys.dat" u (1/$2-T12):(exp(2/$3)*$12) w lp lc rgb '#E41A1C',\
        "res/Ising_2d/Lz0004Lx0128Ly__Vel0016/delta_phys.dat" u (1/$2-T13):(exp(2/$3)*$12) w lp lc rgb '#377EB8',\
        "res/Ising_2d/Lz0004Lx0128Ly__Vel0032/delta_phys.dat" u (1/$2-T14):(exp(2/$3)*$12) w lp lc rgb '#4DAF4A',\
        "res/Ising_2d/Lz0004Lx0128Ly__Vel0064/delta_phys.dat" u (1/$2-T15):(exp(2/$3)*$12) w lp lc rgb '#984EA3',\
        "res/Ising_2d/Lz0004Lx0128Ly__Vel0128/delta_phys.dat" u (1/$2-T16):(exp(2/$3)*$12) w lp lc rgb '#FF7F00'

    set output "eps/vscaled_dfric_Lz0008.eps"
        pl \
        "res/Ising_2d/Lz0008Lx0256Ly__Vel0008/delta_phys.dat" u (1/$2-T22):(exp(2/$3)*$12) w lp lc rgb '#E41A1C',\
        "res/Ising_2d/Lz0008Lx0256Ly__Vel0016/delta_phys.dat" u (1/$2-T23):(exp(2/$3)*$12) w lp lc rgb '#377EB8',\
        "res/Ising_2d/Lz0008Lx0256Ly__Vel0032/delta_phys.dat" u (1/$2-T24):(exp(2/$3)*$12) w lp lc rgb '#4DAF4A',\
        "res/Ising_2d/Lz0008Lx0256Ly__Vel0064/delta_phys.dat" u (1/$2-T25):(exp(2/$3)*$12) w lp lc rgb '#984EA3',\
        "res/Ising_2d/Lz0008Lx0256Ly__Vel0128/delta_phys.dat" u (1/$2-T26):(exp(2/$3)*$12) w lp lc rgb '#FF7F00'

    set output "eps/vscaled_dfric_Lz0016.eps"
        pl \
        "res/Ising_2d/Lz0016Lx0512Ly__Vel0008/delta_phys.dat" u (1/$2-T32):(exp(2/$3)*$12) w lp lc rgb '#E41A1C',\
        "res/Ising_2d/Lz0016Lx0512Ly__Vel0016/delta_phys.dat" u (1/$2-T33):(exp(2/$3)*$12) w lp lc rgb '#377EB8',\
        "res/Ising_2d/Lz0016Lx0512Ly__Vel0032/delta_phys.dat" u (1/$2-T34):(exp(2/$3)*$12) w lp lc rgb '#4DAF4A',\
        "res/Ising_2d/Lz0016Lx0512Ly__Vel0064/delta_phys.dat" u (1/$2-T35):(exp(2/$3)*$12) w lp lc rgb '#984EA3',\
        "res/Ising_2d/Lz0016Lx0512Ly__Vel0128/delta_phys.dat" u (1/$2-T36):(exp(2/$3)*$12) w lp lc rgb '#FF7F00'

    set output "eps/vscaled_dfric_Lz0032.eps"
        pl \
        "res/Ising_2d/Lz0032Lx1024Ly__Vel0008/delta_phys.dat" u (1/$2-T42):(exp(2/$3)*$12) w lp lc rgb '#E41A1C',\
        "res/Ising_2d/Lz0032Lx1024Ly__Vel0016/delta_phys.dat" u (1/$2-T43):(exp(2/$3)*$12) w lp lc rgb '#377EB8',\
        "res/Ising_2d/Lz0032Lx1024Ly__Vel0032/delta_phys.dat" u (1/$2-T44):(exp(2/$3)*$12) w lp lc rgb '#4DAF4A',\
        "res/Ising_2d/Lz0032Lx1024Ly__Vel0064/delta_phys.dat" u (1/$2-T45):(exp(2/$3)*$12) w lp lc rgb '#984EA3',\
        "res/Ising_2d/Lz0032Lx1024Ly__Vel0128/delta_phys.dat" u (1/$2-T46):(exp(2/$3)*$12) w lp lc rgb '#FF7F00'


    set output "eps/vscaled_dfric_LzAll.eps"
        pl \
        "res/Ising_2d/Lz0008Lx0256Ly__Vel0008/delta_phys.dat" u (1/$2-T22):(exp(2/$3)*$12) w lp,\
        "res/Ising_2d/Lz0008Lx0256Ly__Vel0016/delta_phys.dat" u (1/$2-T23):(exp(2/$3)*$12) w lp,\
        "res/Ising_2d/Lz0008Lx0256Ly__Vel0032/delta_phys.dat" u (1/$2-T24):(exp(2/$3)*$12) w lp,\
        "res/Ising_2d/Lz0008Lx0256Ly__Vel0064/delta_phys.dat" u (1/$2-T25):(exp(2/$3)*$12) w lp,\
        "res/Ising_2d/Lz0008Lx0256Ly__Vel0128/delta_phys.dat" u (1/$2-T26):(exp(2/$3)*$12) w lp,\
        "res/Ising_2d/Lz0016Lx0512Ly__Vel0008/delta_phys.dat" u (1/$2-T32):(exp(2/$3)*$12) w lp,\
        "res/Ising_2d/Lz0016Lx0512Ly__Vel0016/delta_phys.dat" u (1/$2-T33):(exp(2/$3)*$12) w lp,\
        "res/Ising_2d/Lz0016Lx0512Ly__Vel0032/delta_phys.dat" u (1/$2-T34):(exp(2/$3)*$12) w lp,\
        "res/Ising_2d/Lz0016Lx0512Ly__Vel0064/delta_phys.dat" u (1/$2-T35):(exp(2/$3)*$12) w lp,\
        "res/Ising_2d/Lz0016Lx0512Ly__Vel0128/delta_phys.dat" u (1/$2-T36):(exp(2/$3)*$12) w lp,\
        "res/Ising_2d/Lz0032Lx1024Ly__Vel0008/delta_phys.dat" u (1/$2-T42):(exp(2/$3)*$12) w lp,\
        "res/Ising_2d/Lz0032Lx1024Ly__Vel0016/delta_phys.dat" u (1/$2-T43):(exp(2/$3)*$12) w lp,\
        "res/Ising_2d/Lz0032Lx1024Ly__Vel0032/delta_phys.dat" u (1/$2-T44):(exp(2/$3)*$12) w lp,\
        "res/Ising_2d/Lz0032Lx1024Ly__Vel0064/delta_phys.dat" u (1/$2-T45):(exp(2/$3)*$12) w lp,\
        "res/Ising_2d/Lz0032Lx1024Ly__Vel0128/delta_phys.dat" u (1/$2-T46):(exp(2/$3)*$12) w lp

# set logscale
set xr [-1.0:1.0]
set yr [0.0:2.0]
# lscaling
    # set yr [0.0:2.0]
    set output "eps/lscaled_dfric_Vel0008.eps"
        pl \
        "res/Ising_2d/Lz0002Lx0064Ly__Vel0008/delta_phys.dat" u ((1/$2-2.9)):(exp($1/b)*$12) w lp,\
        "res/Ising_2d/Lz0004Lx0128Ly__Vel0008/delta_phys.dat" u ((1/$2-2.8)):(exp($1/b)*$12) w lp,\
        "res/Ising_2d/Lz0008Lx0256Ly__Vel0008/delta_phys.dat" u ((1/$2-2.675)):(exp($1/b)*$12) w lp,\
        "res/Ising_2d/Lz0016Lx0512Ly__Vel0008/delta_phys.dat" u ((1/$2-2.6)):(exp($1/b)*$12) w lp

    set output "eps/lscaled_dfric_Vel0016.eps"
        pl \
        "res/Ising_2d/Lz0002Lx0064Ly__Vel0016/delta_phys.dat" u ((1/$2-2.7)):(exp($1/b)*$12) w lp,\
        "res/Ising_2d/Lz0004Lx0128Ly__Vel0016/delta_phys.dat" u ((1/$2-2.75)):(exp($1/b)*$12) w lp,\
        "res/Ising_2d/Lz0008Lx0256Ly__Vel0016/delta_phys.dat" u ((1/$2-2.65)):(exp($1/b)*$12) w lp,\
        "res/Ising_2d/Lz0016Lx0512Ly__Vel0016/delta_phys.dat" u ((1/$2-2.6)):(exp($1/b)*$12) w lp
    
    # set yr [0.0:1.0]
    set output "eps/lscaled_dfric_Vel0032.eps"
        pl \
        "res/Ising_2d/Lz0002Lx0064Ly__Vel0032/delta_phys.dat" u ((1/$2-2.6)):(exp($1/b)*$12) w lp,\
        "res/Ising_2d/Lz0004Lx0128Ly__Vel0032/delta_phys.dat" u ((1/$2-2.70)):(exp($1/b)*$12) w lp,\
        "res/Ising_2d/Lz0008Lx0256Ly__Vel0032/delta_phys.dat" u ((1/$2-2.65)):(exp($1/b)*$12) w lp,\
        "res/Ising_2d/Lz0016Lx0512Ly__Vel0032/delta_phys.dat" u ((1/$2-2.6)):(exp($1/b)*$12) w lp

    set output "eps/lscaled_dfric_Vel0064.eps"
        pl \
        "res/Ising_2d/Lz0002Lx0064Ly__Vel0064/delta_phys.dat" u ((1/$2-2.6)):(exp($1/b)*$12) w lp,\
        "res/Ising_2d/Lz0004Lx0128Ly__Vel0064/delta_phys.dat" u ((1/$2-2.7)):(exp($1/b)*$12) w lp,\
        "res/Ising_2d/Lz0008Lx0256Ly__Vel0064/delta_phys.dat" u ((1/$2-2.65)):(exp($1/b)*$12) w lp,\
        "res/Ising_2d/Lz0016Lx0512Ly__Vel0064/delta_phys.dat" u ((1/$2-2.6)):(exp($1/b)*$12) w lp

    set output "eps/lscaled_dfric_Vel0128.eps"
        pl \
        "res/Ising_2d/Lz0002Lx0064Ly__Vel0128/delta_phys.dat" u ((1/$2-2.5)):(exp($1/b)*$12) w lp,\
        "res/Ising_2d/Lz0004Lx0128Ly__Vel0128/delta_phys.dat" u ((1/$2-2.7)):(exp($1/b)*$12) w lp,\
        "res/Ising_2d/Lz0008Lx0256Ly__Vel0128/delta_phys.dat" u ((1/$2-2.65)):(exp($1/b)*$12) w lp,\
        "res/Ising_2d/Lz0016Lx0512Ly__Vel0128/delta_phys.dat" u ((1/$2-2.6)):(exp($1/b)*$12) w lp