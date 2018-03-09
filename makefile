bin/simness_2d: obj/simness_2d.o obj/mod_proc.o obj/mod_global.o
	ifort obj/simness_2d.o obj/mod_proc.o obj/mod_global.o -o bin/simness_2d -mkl -fopenmp -O3 -ipo
obj/simness_2d.o: src/simness_2d.f90 obj/mod_proc.o obj/mod_global.o
	ifort -c -ipo src/simness_2d.f90 -o obj/simness_2d.o -O3
bin/simes_2d: obj/simes_2d.o obj/mod_proc.o obj/mod_global.o
	ifort obj/simes_2d.o obj/mod_proc.o obj/mod_global.o -o bin/simes_2d -mkl -fopenmp -O3 -ipo
obj/simes_2d.o: src/simes_2d.f90 obj/mod_proc.o obj/mod_global.o
	ifort -c -ipo src/simes_2d.f90 -o obj/simes_2d.o -O3
obj/mod_proc.o: lib/mod_proc.f90 obj/mod_global.o
	ifort -c -ipo lib/mod_proc.f90 -o obj/mod_proc.o -O3
mod/mod_proc.mod: lib/mod_proc.f90 obj/mod_proc.o
	@:
obj/mod_global.o: lib/mod_global.f90
	ifort -c -ipo lib/mod_global.f90 -o obj/mod_global.o
mod/mod_global.mod: lib/mod_global.f90 obj/mod_global.o
	@:
