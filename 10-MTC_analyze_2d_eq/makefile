FC = ifort

ifeq (${FC}, ifort)
	FFLAGS += -I${MKLROOT}/include -O2 -xHOST -ipo -ip
	# FFLAGS += -I${MKLROOT}/include -g -traceback -CB
	LDFLAGS += -mkl -fopenmp
	# LDFLAGS += -mkl
endif

.SUFFIXES:.f90 .o

../bin/MTC_analyze_2d_eq: src/MTC_analyze_2d_eq.o ../lib/mod_proc.o ../lib/mod_global.o
	${FC} -mkl -o $@ $^

.f90.o:
	${FC} ${FFLAGS} ${LDFLAGS} -c -o $@ $<

.PHONY: clean
clean:
	rm -f ../lib/*.o ../lib/*.optrpt src/*.o src/*.optrpt ../bin/* *.mod

src/MTC_analyze_2d_eq.o: src/MTC_analyze_2d_eq.f90 ../lib/mod_proc.o ../lib/mod_global.o
../lib/mod_global.o: ../lib/mod_global.f90
../lib/mod_proc.o: ../lib/mod_proc.f90