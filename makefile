bin/simness_2d: obj/simness_2d.o obj/mod_proc.o obj/mod_global.o
	ifort obj/simness_2d.o obj/mod_proc.o obj/mod_global.o -o bin/simness_2d -mkl -parallel -fopenmp -fast
obj/simness_2d.o: src/simness_2d.f90 obj/mod_proc.o obj/mod_global.o
	ifort -c -ipo src/simness_2d.f90 -o obj/simness_2d.o -mkl -parallel -fopenmp -fast
bin/simes_2d: obj/simes_2d.o obj/mod_proc.o obj/mod_global.o
	ifort obj/simes_2d.o obj/mod_proc.o obj/mod_global.o -o bin/simes_2d -mkl -parallel -fopenmp -fast
obj/simes_2d.o: src/simes_2d.f90 obj/mod_proc.o obj/mod_global.o
	ifort -c -ipo src/simes_2d.f90 -o obj/simes_2d.o -mkl -parallel -fopenmp -fast
bin/calcness_2d: obj/calcness_2d.o obj/mod_proc.o obj/mod_global.o
		ifort obj/calcness_2d.o obj/mod_proc.o obj/mod_global.o -o bin/calcness_2d -mkl -parallel -fopenmp -fast
obj/calcness_2d.o: src/calcness_2d.f90 obj/mod_proc.o obj/mod_global.o
		ifort -c -ipo src/calcness_2d.f90 -o obj/calcness_2d.o -mkl -parallel -fopenmp -fast
bin/corrness_2d: obj/corrness_2d.o obj/mod_proc.o obj/mod_global.o
		ifort obj/corrness_2d.o obj/mod_proc.o obj/mod_global.o -o bin/corrness_2d -mkl -parallel -fopenmp -fast
obj/corrness_2d.o: src/corrness_2d.f90 obj/mod_proc.o obj/mod_global.o
		ifort -c -ipo src/corrness_2d.f90 -o obj/corrness_2d.o -mkl -parallel -fopenmp -fast
obj/mod_proc.o: lib/mod_proc.f90 obj/mod_global.o
	ifort -c -ipo lib/mod_proc.f90 -o obj/mod_proc.o -mkl -parallel -fopenmp -fast
mod/mod_proc.mod: lib/mod_proc.f90 obj/mod_proc.o
	@:
obj/mod_global.o: lib/mod_global.f90
	ifort -c -ipo lib/mod_global.f90 -o obj/mod_global.o
mod/mod_global.mod: lib/mod_global.f90 obj/mod_global.o
	@:
