bin/simulate_2d: obj/simulate_2d.o obj/mod_proc_int.o obj/mod_proc_file.o obj/mod_rand.o obj/mod_global.o
	ifort obj/simulate_2d.o obj/mod_proc_int.o obj/mod_proc_file.o obj/mod_rand.o obj/mod_global.o -o bin/simulate_2d -mkl
obj/simulate_2d.o: src/simulate_2d.f90 obj/mod_proc_int.o obj/mod_proc_file.o obj/mod_rand.o obj/mod_global.o
	ifort -c src/simulate_2d.f90 -o obj/simulate_2d.o
obj/mod_proc_int.o: lib/mod_proc_int.f90 obj/mod_global.o obj/mod_rand.o
	ifort -c lib/mod_proc_int.f90 -o obj/mod_proc_int.o
mod/mod_proc_int.mod: lib/mod_proc_int.f90 obj/mod_proc_int.o
	@: 
obj/mod_proc_file.o: lib/mod_proc_file.f90 obj/mod_global.o obj/mod_rand.o
	ifort -c lib/mod_proc_file.f90 -o obj/mod_proc_file.o
mod/mod_proc_file.mod: lib/mod_proc_file.f90 obj/mod_proc_file.o
	@:
obj/mod_rand.o: lib/mod_rand.f90 obj/mod_global.o
	ifort -c lib/mod_rand.f90 -o obj/mod_rand.o -mkl
mod/mod_rand.mod: lib/mod_rand.f90 obj/mod_rand.o
	@:
obj/mod_global.o: lib/mod_global.f90
	ifort -c lib/mod_global.f90 -o obj/mod_global.o -mkl
mod/mod_global.mod: lib/mod_global.f90 obj/mod_global.o
	@:
