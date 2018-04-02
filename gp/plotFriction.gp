set terminal postscript eps enhanced color
set grid

# Lz=0002
    set output "eps/fric_Lz0002Vel0008.eps"
        pl \
        "dat/Lz0002Lx0064Ly__Vel0008/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0002Lx0064Ly__Vel0008/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0002Lx0064Ly__Vel0008/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

    set output "eps/fric_Lz0002Vel0016.eps"
        pl \
        "dat/Lz0002Lx0064Ly__Vel0016/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0002Lx0064Ly__Vel0016/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0002Lx0064Ly__Vel0016/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

    set output "eps/fric_Lz0002Vel0032.eps"
        pl \
        "dat/Lz0002Lx0064Ly__Vel0032/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0002Lx0064Ly__Vel0032/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0002Lx0064Ly__Vel0032/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

    set output "eps/fric_Lz0002Vel0064.eps"
        pl \
        "dat/Lz0002Lx0064Ly__Vel0064/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0002Lx0064Ly__Vel0064/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0002Lx0064Ly__Vel0064/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

    set output "eps/fric_Lz0002Vel0128.eps"
        pl \
        "dat/Lz0002Lx0064Ly__Vel0128/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0002Lx0064Ly__Vel0128/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0002Lx0064Ly__Vel0128/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

# Lz=0004
    set output "eps/fric_Lz0004Vel0008.eps"
        pl \
        "dat/Lz0004Lx0128Ly__Vel0008/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0004Lx0128Ly__Vel0008/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0004Lx0128Ly__Vel0008/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

    set output "eps/fric_Lz0004Vel0016.eps"
        pl \
        "dat/Lz0004Lx0128Ly__Vel0016/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0004Lx0128Ly__Vel0016/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0004Lx0128Ly__Vel0016/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

    set output "eps/fric_Lz0004Vel0032.eps"
        pl \
        "dat/Lz0004Lx0128Ly__Vel0032/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0004Lx0128Ly__Vel0032/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0004Lx0128Ly__Vel0032/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

    set output "eps/fric_Lz0004Vel0064.eps"
        pl \
        "dat/Lz0004Lx0128Ly__Vel0064/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0004Lx0128Ly__Vel0064/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0004Lx0128Ly__Vel0064/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

    set output "eps/fric_Lz0004Vel0128.eps"
        pl \
        "dat/Lz0004Lx0128Ly__Vel0128/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0004Lx0128Ly__Vel0128/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0004Lx0128Ly__Vel0128/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

# Lz=0008
    set output "eps/fric_Lz0008Vel0008.eps"
        pl \
        "dat/Lz0008Lx0256Ly__Vel0008/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0008Lx0256Ly__Vel0008/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0008Lx0256Ly__Vel0008/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

    set output "eps/fric_Lz0008Vel0016.eps"
        pl \
        "dat/Lz0008Lx0256Ly__Vel0016/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0008Lx0256Ly__Vel0016/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0008Lx0256Ly__Vel0016/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

    set output "eps/fric_Lz0008Vel0032.eps"
        pl \
        "dat/Lz0008Lx0256Ly__Vel0032/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0008Lx0256Ly__Vel0032/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0008Lx0256Ly__Vel0032/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

    set output "eps/fric_Lz0008Vel0064.eps"
        pl \
        "dat/Lz0008Lx0256Ly__Vel0064/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0008Lx0256Ly__Vel0064/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0008Lx0256Ly__Vel0064/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

    set output "eps/fric_Lz0008Vel0128.eps"
        pl \
        "dat/Lz0008Lx0256Ly__Vel0128/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0008Lx0256Ly__Vel0128/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0008Lx0256Ly__Vel0128/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

# Lz=0016
    set output "eps/fric_Lz0016Vel0008.eps"
        pl \
        "dat/Lz0016Lx0512Ly__Vel0008/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0016Lx0512Ly__Vel0008/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0016Lx0512Ly__Vel0008/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

    set output "eps/fric_Lz0016Vel0016.eps"
        pl \
        "dat/Lz0016Lx0512Ly__Vel0016/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0016Lx0512Ly__Vel0016/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0016Lx0512Ly__Vel0016/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

    set output "eps/fric_Lz0016Vel0032.eps"
        pl \
        "dat/Lz0016Lx0512Ly__Vel0032/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0016Lx0512Ly__Vel0032/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0016Lx0512Ly__Vel0032/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

    set output "eps/fric_Lz0016Vel0064.eps"
        pl \
        "dat/Lz0016Lx0512Ly__Vel0064/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0016Lx0512Ly__Vel0064/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0016Lx0512Ly__Vel0064/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

    set output "eps/fric_Lz0016Vel0128.eps"
        pl \
        "dat/Lz0016Lx0512Ly__Vel0128/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0016Lx0512Ly__Vel0128/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0016Lx0512Ly__Vel0128/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

# Lz=0032
    set output "eps/fric_Lz0032Vel0008.eps"
        pl \
        "dat/Lz0032Lx1024Ly__Vel0008/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0032Lx1024Ly__Vel0008/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0032Lx1024Ly__Vel0008/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

    set output "eps/fric_Lz0032Vel0016.eps"
        pl \
        "dat/Lz0032Lx1024Ly__Vel0016/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0032Lx1024Ly__Vel0016/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0032Lx1024Ly__Vel0016/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

    set output "eps/fric_Lz0032Vel0032.eps"
        pl \
        "dat/Lz0032Lx1024Ly__Vel0032/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0032Lx1024Ly__Vel0032/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0032Lx1024Ly__Vel0032/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

    set output "eps/fric_Lz0032Vel0064.eps"
        pl \
        "dat/Lz0032Lx1024Ly__Vel0064/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0032Lx1024Ly__Vel0064/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0032Lx1024Ly__Vel0064/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

    set output "eps/fric_Lz0032Vel0128.eps"
        pl \
        "dat/Lz0032Lx1024Ly__Vel0128/01-antiparallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0032Lx1024Ly__Vel0128/02-parallel/physquan1.dat" u (1/$2):12:13 with errorbars,\
        "dat/Lz0032Lx1024Ly__Vel0128/03-free/physquan1.dat" u (1/$2):12:13 with errorbars

# vscaling
    set output "eps/vscaled_fric_Lz0002.eps"
        pl \
        "dat/Lz0002Lx0064Ly__Vel0008/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0002Lx0064Ly__Vel0016/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0002Lx0064Ly__Vel0032/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0002Lx0064Ly__Vel0064/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0002Lx0064Ly__Vel0128/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0002Lx0064Ly__Vel0008/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0002Lx0064Ly__Vel0016/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0002Lx0064Ly__Vel0032/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0002Lx0064Ly__Vel0064/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0002Lx0064Ly__Vel0128/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0002Lx0064Ly__Vel0008/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green",\
        "dat/Lz0002Lx0064Ly__Vel0016/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green",\
        "dat/Lz0002Lx0064Ly__Vel0032/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green",\
        "dat/Lz0002Lx0064Ly__Vel0064/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green",\
        "dat/Lz0002Lx0064Ly__Vel0128/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green"

    set output "eps/vscaled_fric_Lz0004.eps"
        pl \
        "dat/Lz0004Lx0128Ly__Vel0008/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0004Lx0128Ly__Vel0016/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0004Lx0128Ly__Vel0032/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0004Lx0128Ly__Vel0064/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0004Lx0128Ly__Vel0128/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0004Lx0128Ly__Vel0008/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0004Lx0128Ly__Vel0016/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0004Lx0128Ly__Vel0032/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0004Lx0128Ly__Vel0064/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0004Lx0128Ly__Vel0128/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0004Lx0128Ly__Vel0008/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green",\
        "dat/Lz0004Lx0128Ly__Vel0016/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green",\
        "dat/Lz0004Lx0128Ly__Vel0032/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green",\
        "dat/Lz0004Lx0128Ly__Vel0064/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green",\
        "dat/Lz0004Lx0128Ly__Vel0128/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green"
    
    set output "eps/vscaled_fric_Lz0008.eps"
        pl \
        "dat/Lz0008Lx0256Ly__Vel0008/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0008Lx0256Ly__Vel0016/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0008Lx0256Ly__Vel0032/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0008Lx0256Ly__Vel0064/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0008Lx0256Ly__Vel0128/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0008Lx0256Ly__Vel0008/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0008Lx0256Ly__Vel0016/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0008Lx0256Ly__Vel0032/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0008Lx0256Ly__Vel0064/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0008Lx0256Ly__Vel0128/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0008Lx0256Ly__Vel0008/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green",\
        "dat/Lz0008Lx0256Ly__Vel0016/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green",\
        "dat/Lz0008Lx0256Ly__Vel0032/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green",\
        "dat/Lz0008Lx0256Ly__Vel0064/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green",\
        "dat/Lz0008Lx0256Ly__Vel0128/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green"

    set output "eps/vscaled_fric_Lz0016.eps"
        pl \
        "dat/Lz0016Lx0512Ly__Vel0008/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0016Lx0512Ly__Vel0016/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0016Lx0512Ly__Vel0032/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0016Lx0512Ly__Vel0064/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0016Lx0512Ly__Vel0128/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0016Lx0512Ly__Vel0008/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0016Lx0512Ly__Vel0016/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0016Lx0512Ly__Vel0032/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0016Lx0512Ly__Vel0064/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0016Lx0512Ly__Vel0128/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0016Lx0512Ly__Vel0008/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green",\
        "dat/Lz0016Lx0512Ly__Vel0016/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green",\
        "dat/Lz0016Lx0512Ly__Vel0032/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green",\
        "dat/Lz0016Lx0512Ly__Vel0064/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green",\
        "dat/Lz0016Lx0512Ly__Vel0128/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green"

    set output "eps/vscaled_fric_Lz0032.eps"
        pl \
        "dat/Lz0032Lx1024Ly__Vel0008/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0032Lx1024Ly__Vel0016/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0032Lx1024Ly__Vel0032/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0032Lx1024Ly__Vel0064/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0032Lx1024Ly__Vel0128/01-antiparallel/physquan1.dat" u (1/$2):(exp(2/$3)*$12) lc rgb "orange-red"  ,\
        "dat/Lz0032Lx1024Ly__Vel0008/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0032Lx1024Ly__Vel0016/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0032Lx1024Ly__Vel0032/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0032Lx1024Ly__Vel0064/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0032Lx1024Ly__Vel0128/02-parallel/physquan1.dat"     u (1/$2):(exp(2/$3)*$12) lc rgb "navy"        ,\
        "dat/Lz0032Lx1024Ly__Vel0008/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green",\
        "dat/Lz0032Lx1024Ly__Vel0016/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green",\
        "dat/Lz0032Lx1024Ly__Vel0032/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green",\
        "dat/Lz0032Lx1024Ly__Vel0064/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green",\
        "dat/Lz0032Lx1024Ly__Vel0128/03-free/physquan1.dat"         u (1/$2):(exp(2/$3)*$12) lc rgb "forest-green"