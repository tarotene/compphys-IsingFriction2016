main_procedures.mod : \
  lib/002_main_procedures.f90

obj/002_main_procedures.o : \
  lib/002_main_procedures.f90 ./obj/mod_rng.mod ./obj/mkl_vsl.mod ./obj/mkl_vsl_type.mod \
  ./obj/global_variables.mod

