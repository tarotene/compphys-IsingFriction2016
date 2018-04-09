FC = ifort
ifeq (${FC}, ifort)
	FFLAGS += -I${MKLROOT}/include
	LDFLAGS += ${MKLROOT}/lib/libmkl_intel_lp64.a ${MKLROOT}/lib/libmkl_intel_thread.a ${MKLROOT}/lib/libmkl_core.a -liomp5 -lpthread -lm -ldl
endif

bin/Ising_2d: src/Ising_2d.o lib/mod_proc.o lib/mod_global.o
	${FC} ${LDFLAGS} -o $@ $^
bin/Ising_3d: src/Ising_3d.o lib/mod_proc.o lib/mod_global.o
	${FC} ${LDFLAGS} -o $@ $^
bin/Ising_2d_eq: src/Ising_2d_eq.o lib/mod_proc.o lib/mod_global.o
	${FC} ${LDFLAGS} -o $@ $^
bin/Ising_3d_eq: src/Ising_3d_eq.o lib/mod_proc.o lib/mod_global.o
	${FC} ${LDFLAGS} -o $@ $^
bin/corr_2d: src/corr_2d.o lib/mod_proc.o lib/mod_global.o
	${FC} ${LDFLAGS} -o $@ $^
bin/corr_3d: src/corr_3d.o lib/mod_proc.o lib/mod_global.o
	${FC} ${LDFLAGS} -o $@ $^
bin/calc_2d: src/calc_2d.o lib/mod_proc.o lib/mod_global.o
	${FC} ${LDFLAGS} -o $@ $^
bin/calc_3d: src/calc_3d.o lib/mod_proc.o lib/mod_global.o
	${FC} ${LDFLAGS} -o $@ $^
bin/corr_2d_eq: src/corr_2d_eq.o lib/mod_proc.o lib/mod_global.o
	${FC} ${LDFLAGS} -o $@ $^
bin/corr_3d_eq: src/corr_3d_eq.o lib/mod_proc.o lib/mod_global.o
	${FC} ${LDFLAGS} -o $@ $^
bin/calc_2d_eq: src/calc_2d_eq.o lib/mod_proc.o lib/mod_global.o
	${FC} ${LDFLAGS} -o $@ $^
bin/calc_3d_eq: src/calc_3d_eq.o lib/mod_proc.o lib/mod_global.o
	${FC} ${LDFLAGS} -o $@ $^

src/%.o : src/%.f90 lib/mod_proc.o lib/mod_global.o
	${FC} ${FFLAGS} -c $< -o $@
lib/mod_global.o : lib/mod_global.f90
	${FC} ${FFLAGS} -c $< -o $@
lib/mod_proc.o : lib/mod_proc.f90 lib/mod_global.o
	${FC} ${FFLAGS} -c $< -o $@

.PHONY: all
all: bin/Ising_2d bin/Ising_3d bin/Ising_2d_eq bin/Ising_3d_eq bin/corr_2d bin/corr_3d bin/calc_2d bin/calc_3d bin/corr_2d_eq bin/corr_3d_eq bin/calc_2d_eq bin/calc_3d_eq
