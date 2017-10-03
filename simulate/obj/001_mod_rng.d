mod_rng.mod : \
  lib/001_mod_rng.f90

obj/001_mod_rng.o : \
  lib/001_mod_rng.f90 ./obj/mkl_vsl.mod ./obj/mkl_vsl_type.mod ./obj/global_variables.mod

