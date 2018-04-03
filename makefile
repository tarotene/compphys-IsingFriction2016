bin/Ising_2d: obj/Ising_2d.o obj/mod_proc.o obj/mod_global.o
	ifort obj/Ising_2d.o obj/mod_proc.o obj/mod_global.o -o bin/Ising_2d -mkl -parallel -fopenmp -fast
obj/Ising_2d.o: src/Ising_2d.f90 obj/mod_proc.o obj/mod_global.o
	ifort -c -ipo src/Ising_2d.f90 -o obj/Ising_2d.o -mkl -parallel -fopenmp -fast
bin/Ising_3d: obj/Ising_3d.o obj/mod_proc.o obj/mod_global.o
	ifort obj/Ising_3d.o obj/mod_proc.o obj/mod_global.o -o bin/Ising_3d -mkl -parallel -fopenmp -fast
obj/Ising_3d.o: src/Ising_3d.f90 obj/mod_proc.o obj/mod_global.o
	ifort -c -ipo src/Ising_3d.f90 -o obj/Ising_3d.o -mkl -parallel -fopenmp -fast
bin/Ising_2d_eq: obj/Ising_2d_eq.o obj/mod_proc.o obj/mod_global.o
	ifort obj/Ising_2d_eq.o obj/mod_proc.o obj/mod_global.o -o bin/Ising_2d_eq -mkl -parallel -fopenmp -fast
obj/Ising_2d_eq.o: src/Ising_2d_eq.f90 obj/mod_proc.o obj/mod_global.o
	ifort -c -ipo src/Ising_2d_eq.f90 -o obj/Ising_2d_eq.o -mkl -parallel -fopenmp -fast
bin/Ising_3d_eq: obj/Ising_3d_eq.o obj/mod_proc.o obj/mod_global.o
	ifort obj/Ising_3d_eq.o obj/mod_proc.o obj/mod_global.o -o bin/Ising_3d_eq -mkl -parallel -fopenmp -fast
obj/Ising_3d_eq.o: src/Ising_3d_eq.f90 obj/mod_proc.o obj/mod_global.o
	ifort -c -ipo src/Ising_3d_eq.f90 -o obj/Ising_3d_eq.o -mkl -parallel -fopenmp -fast
bin/calc_2d: obj/calc_2d.o obj/mod_proc.o obj/mod_global.o
	ifort obj/calc_2d.o obj/mod_proc.o obj/mod_global.o -o bin/calc_2d -mkl -parallel -fopenmp -fast
obj/calc_2d.o: src/calc_2d.f90 obj/mod_proc.o obj/mod_global.o
	ifort -c -ipo src/calc_2d.f90 -o obj/calc_2d.o -mkl -parallel -fopenmp -fast
bin/corr_2d: obj/corr_2d.o obj/mod_proc.o obj/mod_global.o
	ifort obj/corr_2d.o obj/mod_proc.o obj/mod_global.o -o bin/corr_2d -mkl -parallel -fopenmp -fast
obj/corr_2d.o: src/corr_2d.f90 obj/mod_proc.o obj/mod_global.o
	ifort -c -ipo src/corr_2d.f90 -o obj/corr_2d.o -mkl -parallel -fopenmp -fast
obj/mod_proc.o: lib/mod_proc.f90 obj/mod_global.o
	ifort -c -ipo lib/mod_proc.f90 -o obj/mod_proc.o -mkl -parallel -fopenmp -fast
mod/mod_proc.mod: lib/mod_proc.f90 obj/mod_proc.o
	@:
obj/mod_global.o: lib/mod_global.f90
	ifort -c -ipo lib/mod_global.f90 -o obj/mod_global.o
mod/mod_global.mod: lib/mod_global.f90 obj/mod_global.o
	@:
