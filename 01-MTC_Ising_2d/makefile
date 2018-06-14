FC = ifort

ifeq (${FC}, ifort)
	FFLAGS += -I${MKLROOT}/include -O2 -xHOST -ipo
	# FFLAGS += -I${MKLROOT}/include -g -traceback -CB
	LDFLAGS += -mkl -fopenmp
	# LDFLAGS += -mkl
endif

.SUFFIXES:.f90 .o

../bin/MTC_Ising_2d: src/MTC_Ising_2d.o ../lib/mod_proc.o ../lib/mod_global.o
	${FC} -mkl -o $@ $^

.f90.o:
	${FC} ${FFLAGS} ${LDFLAGS} -c -o $@ $<

.PHONY: clean
clean:
	rm -f ../lib/*.o ../lib/*.optrpt src/*.o src/*.optrpt ../bin/* *.mod

src/MTC_Ising_2d.o: src/MTC_Ising_2d.f90 ../lib/mod_proc.o ../lib/mod_global.o
../lib/mod_global.o: ../lib/mod_global.f90
../lib/mod_proc.o: ../lib/mod_proc.f90
