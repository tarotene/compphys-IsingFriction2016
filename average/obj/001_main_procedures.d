main_procedures.mod : \
  lib/001_main_procedures.f90

obj/001_main_procedures.o : \
  lib/001_main_procedures.f90 \
  /opt/intel/compilers_and_libraries_2018.0.104/mac/compiler/include/intel64/ifport_types.mod \
  /opt/intel/compilers_and_libraries_2018.0.104/mac/compiler/include/intel64/ifport.mod \
  ./obj/global_variables.mod

