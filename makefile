FC = ifort

ifeq (${FC}, ifort)
	FFLAGS += -I${MKLROOT}/include -O3 -xHOST -ipo -ip
	# FFLAGS += -I${MKLROOT}/include -g -traceback -CB
	LDFLAGS += -mkl -fopenmp
	# LDFLAGS += -mkl
endif

.SUFFIXES:.f90 .o .bin

.f90.o:
	${FC} ${FFLAGS} ${LDFLAGS} -c -o $@ $<

.PHONY: all
all: src/MTC_analyze_2d_eq.o src/MTC_Ising_2d_eq.o

.PHONY: clean
clean:
	rm -f lib/*.o src/*.o bin/* *.mod

src/MTC_analyze_2d_eq.o : src/MTC_analyze_2d_eq.f90 lib/mod_proc.o lib/mod_global.o
src/MTC_Ising_2d_eq.o : src/MTC_Ising_2d_eq.f90 lib/mod_proc.o lib/mod_global.o
lib/mod_global.o : lib/mod_global.f90
lib/mod_proc.o : lib/mod_proc.f90
