#!/usr/bin/env bash

source /opt/intel/mkl/bin/mklvars.sh intel64 mod ilp64

J=1.0; n_samples=100; n_sweeps_therm=100; n_sweeps_stead=100

echo "What to schedule?"
echo "(2017/10/12: {average, delete, extract, simulate} are available.)"
read scheduled

cd dat
for dir in `ls`; do
  cd ${dir}

  len_z=`pwd | sed -e "s/^.*Lz\(.*\)Lx.*$/\1/"`
  len_x=`pwd | sed -e "s/^.*Lx\(.*\)_Vel.*$/\1/"`
  vel=`pwd | sed -e "s/^.*_Vel\(.*\).*$/\1/"`

  cd antiparallel
  id_init=2
  id_bound=1
  source ../../../sh/${scheduled}.sh
  echo "Done: z-bc = antiparallel"
  cd ../

  cd parallel
  id_init=1
  id_bound=2
  source ../../../sh/${scheduled}.sh
  echo "Done: z-bc = parallel"
  cd ../

  echo "Done: len_x = ${len_x}, len_z = ${len_z}."

  cd ../
done
cd ../
