bin/simness_2d: obj/simness_2d.o obj/mod_proc.o obj/mod_global.o
	ifort obj/simness_2d.o obj/mod_proc.o obj/mod_global.o -o bin/simness_2d
obj/simness_2d.o: src/simness_2d.f90 obj/mod_proc.o obj/mod_global.o
	ifort -c src/simness_2d.f90 -o obj/simness_2d.o -fopenmp -O3
obj/mod_proc.o: lib/mod_proc.f90 obj/mod_global.o
	ifort -c lib/mod_proc.f90 -o obj/mod_proc.o -O3 -mkl
mod/mod_proc.mod: lib/mod_proc.f90 obj/mod_proc.o
	@:
obj/mod_global.o: lib/mod_global.f90
	ifort -c lib/mod_global.f90 -o obj/mod_global.o -O3
mod/mod_global.mod: lib/mod_global.f90 obj/mod_global.o
	@:
