FC = ifort
ifeq (${FC}, ifort)
	FFLAGS += -I${MKLROOT}/include
	LDFLAGS += ${MKLROOT}/lib/libmkl_intel_lp64.a ${MKLROOT}/lib/libmkl_intel_thread.a ${MKLROOT}/lib/libmkl_core.a -liomp5 -lpthread -lm -ldl
endif
OBJS = obj/Ising_3d_eq.o obj/mod_proc.o obj/mod_global.o

Ising_3d_eq: ${OBJS}
	${FC} ${FFLAGS} ${LDFLAGS} -o ./bin/Ising_3d_eq ${OBJS}

obj/Ising_3d_eq.o: src/Ising_3d_eq.f90 obj/mod_proc.o obj/mod_global.o
	${FC} ${FFLAGS} -c src/Ising_3d_eq.f90 -o obj/Ising_3d_eq.o

obj/mod_proc.o: lib/mod_proc.f90 obj/mod_global.o
	${FC} ${FFLAGS} -c lib/mod_proc.f90 -o obj/mod_proc.o

obj/mod_global.o: lib/mod_global.f90
	${FC} ${FFLAGS} -c lib/mod_global.f90 -o obj/mod_global.o