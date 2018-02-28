bin/simulate_2d: obj/simulate_2d.o obj/mod_proc.o obj/mod_global.o
	ifort obj/simulate_2d.o obj/mod_proc.o obj/mod_global.o -o bin/simulate_2d -mkl
obj/simulate_2d.o: src/simulate_2d.f90 obj/mod_proc.o obj/mod_global.o
	ifort -c src/simulate_2d.f90 -o obj/simulate_2d.o -fopenmp
obj/mod_proc.o: lib/mod_proc.f90 obj/mod_global.o
	ifort -c lib/mod_proc.f90 -o obj/mod_proc.o
mod/mod_proc.mod: lib/mod_proc.f90 obj/mod_proc.o
	@:
obj/mod_global.o: lib/mod_global.f90
	ifort -c lib/mod_global.f90 -o obj/mod_global.o -mkl
mod/mod_global.mod: lib/mod_global.f90 obj/mod_global.o
	@:
