#!/usr/bin/env bash

source /opt/intel/mkl/bin/mklvars.sh intel64 mod ilp64

n_samples=10; n_sweep_therm=10; n_sweep_stead=10

cd dat
for dir in `ls`; do
  cd ${dir}

  len_z=`pwd | sed -e "s/^.*Lz\(.*\)Lx.*$/\1/"`; J=1.0
  vel=`pwd | sed -e "s/^.*_Vel\(.*\).*$/\1/"`
  len_x=`pwd | sed -e "s/^.*Lx\(.*\)_Vel.*$/\1/"`

  id_init=2; id_bound=3 #antiparallel
  cd antiparallel
  source ../../../sh/simulate.sh
  cd ../

  id_init=1; id_bound=2 #parallel
  cd parallel
  source ../../../sh/simulate.sh
  cd ../

  cd ../
done
