FC = ifort

ifeq (${FC}, ifort)
	FFLAGS += -I${MKLROOT}/include -parallel -O3 -xHOST -ip -ipo
	LDFLAGS += -mkl -fopenmp -ipo
endif

bin/Ising_2d: src/Ising_2d.o lib/mod_proc.o lib/mod_global.o
	${FC} ${LDFLAGS} -o $@ $^
bin/Ising_2d_RESUME: src/Ising_2d_RESUME.o lib/mod_proc.o lib/mod_global.o
	${FC} ${LDFLAGS} -o $@ $^
bin/Ising_3d: src/Ising_3d.o lib/mod_proc.o lib/mod_global.o
	${FC} ${LDFLAGS} -o $@ $^
bin/Ising_2d_eq: src/Ising_2d_eq.o lib/mod_proc.o lib/mod_global.o
	${FC} ${LDFLAGS} -o $@ $^
bin/Ising_2d_eq_RESUME: src/Ising_2d_eq_RESUME.o lib/mod_proc.o lib/mod_global.o
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
lib/mod_proc.o : lib/mod_proc.f90 lib/mod_global.o
	${FC} ${FFLAGS} -c $< -o $@
lib/mod_global.o : lib/mod_global.f90
	${FC} ${FFLAGS} -c $< -o $@

.PHONY: all
all: bin/Ising_2d bin/Ising_2d_RESUME bin/Ising_3d bin/Ising_2d_eq_RESUME bin/Ising_3d_eq bin/corr_2d bin/corr_3d bin/calc_2d bin/calc_3d bin/corr_2d_eq bin/corr_3d_eq bin/calc_2d_eq bin/calc_3d_eq

.PHONY: clean
clean:
	rm -f bin/Ising_2d{,_RESUME} bin/Ising_3d bin/Ising_2d_eq{,_RESUME} bin/Ising_3d_eq bin/corr_2d bin/corr_3d bin/calc_2d bin/calc_3d bin/corr_2d_eq bin/corr_3d_eq bin/calc_2d_eq bin/calc_3d_eq
	rm -f src/Ising_2d{,_RESUME}.o src/Ising_3d.o src/Ising_2d_eq{,_RESUME}.o src/Ising_3d_eq.o src/corr_2d.o src/corr_3d.o src/corr_2d_eq.o src/corr_3d_eq.o src/calc_2d.o src/calc_3d.o src/calc_2d_eq.o src/calc_3d_eq.o lib/mod_global.o lib/mod_proc.o
	rm -f mkl_vsl{,_type}.mod mod_{global,proc}.mod
