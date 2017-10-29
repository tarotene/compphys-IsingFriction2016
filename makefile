bin/simulate_2d: obj/simulate_2d.o obj/mod_proc_int.o obj/mod_proc_file.o obj/mod_rand.o obj/mod_global.o
	ifort obj/simulate_2d.o obj/mod_proc_int.o obj/mod_proc_file.o obj/mod_rand.o obj/mod_global.o -o bin/simulate_2d -mkl
bin/average_2d: obj/average_2d.o obj/mod_proc_int.o obj/mod_proc_file.o obj/mod_rand.o obj/mod_global.o
	ifort obj/average_2d.o obj/mod_proc_int.o obj/mod_proc_file.o obj/mod_rand.o obj/mod_global.o -o bin/average_2d -mkl
bin/extract_2d: obj/extract_2d.o obj/mod_proc_int.o obj/mod_proc_file.o obj/mod_rand.o obj/mod_global.o
	ifort obj/extract_2d.o obj/mod_proc_int.o obj/mod_proc_file.o obj/mod_rand.o obj/mod_global.o -o bin/extract_2d -mkl
bin/delete_2d: obj/delete_2d.o obj/mod_proc_file.o obj/mod_global.o
	ifort obj/delete_2d.o obj/mod_proc_file.o obj/mod_global.o -o bin/delete_2d
obj/simulate_2d.o: src/simulate_2d.f90 obj/mod_proc_int.o obj/mod_proc_file.o obj/mod_rand.o obj/mod_global.o
	ifort -c src/simulate_2d.f90 -o obj/simulate_2d.o -fopenmp
obj/average_2d.o: src/average_2d.f90 obj/mod_proc_int.o obj/mod_proc_file.o obj/mod_rand.o obj/mod_global.o
	ifort -c src/average_2d.f90 -o obj/average_2d.o -fopenmp
obj/extract_2d.o: src/extract_2d.f90 obj/mod_proc_int.o obj/mod_proc_file.o obj/mod_rand.o obj/mod_global.o
	ifort -c src/extract_2d.f90 -o obj/extract_2d.o -fopenmp
obj/delete_2d.o: src/delete_2d.f90 obj/mod_proc_file.o obj/mod_global.o
	ifort -c src/delete_2d.f90 -o obj/delete_2d.o
obj/mod_proc_int.o: lib/mod_proc_int.f90 obj/mod_global.o obj/mod_rand.o
	ifort -c lib/mod_proc_int.f90 -o obj/mod_proc_int.o
mod/mod_proc_int.mod: lib/mod_proc_int.f90 obj/mod_proc_int.o
	@:
obj/mod_proc_file.o: lib/mod_proc_file.f90 obj/mod_global.o
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
