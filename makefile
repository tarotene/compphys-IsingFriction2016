FC = ifort

ifeq (${FC}, ifort)
	FFLAGS += -I${MKLROOT}/include -O3 -xHOST -ipo -ip
	# FFLAGS += -I${MKLROOT}/include -g -traceback -CB
	LDFLAGS += ${MKLROOT}/lib/libmkl_intel_lp64.a ${MKLROOT}/lib/libmkl_intel_thread.a ${MKLROOT}/lib/libmkl_core.a -liomp5 -lpthread -lm -ldl -fopenmp
	# LDFLAGS += ${MKLROOT}/lib/libmkl_intel_lp64.a ${MKLROOT}/lib/libmkl_intel_thread.a ${MKLROOT}/lib/libmkl_core.a -liomp5 -lpthread -lm -ldl
endif

.SUFFIXES:.f90 .o

bin/MTC_Ising_2d_eq: src/MTC_Ising_2d_eq.o lib/mod_proc.o lib/mod_global.o
	${FC} -mkl -o $@ $^
bin/MTC_Ising_2d: src/MTC_Ising_2d.o lib/mod_proc.o lib/mod_global.o
	${FC} -mkl -o $@ $^
bin/MTC_analyze_2d_eq: src/MTC_analyze_2d_eq.o lib/mod_proc.o lib/mod_global.o
	${FC} -mkl -o $@ $^
bin/MTC_analyze_2d: src/MTC_analyze_2d.o lib/mod_proc.o lib/mod_global.o
	${FC} -mkl -o $@ $^

.f90.o:
	${FC} ${FFLAGS} ${LDFLAGS} -c -o $@ $<

.PHONY: clean
clean:
	rm -f lib/*.o lib/*.optrpt src/*.o src/*.optrpt bin/* *.mod

src/MTC_Ising_2d_eq.o: src/MTC_Ising_2d_eq.f90 lib/mod_proc.o lib/mod_global.o
src/MTC_Ising_2d.o: src/MTC_Ising_2d.f90 lib/mod_proc.o lib/mod_global.o
src/MTC_analyze_2d_eq.o: src/MTC_analyze_2d_eq.f90 lib/mod_proc.o lib/mod_global.o
src/MTC_analyze_2d.o: src/MTC_analyze_2d.f90 lib/mod_proc.o lib/mod_global.o
lib/mod_global.o: lib/mod_global.f90
lib/mod_proc.o: lib/mod_proc.f90