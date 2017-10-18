#!/usr/bin/env bash

source /opt/intel/mkl/bin/mklvars.sh intel64 mod ilp64

cd dat
for dir in `ls`; do
  cd ${dir}

  len_z=`pwd | sed -e "s/^.*Lz\(.*\)Lx.*$/\1/"`
  len_x=`pwd | sed -e "s/^.*Lx\(.*\)_Vel.*$/\1/"`

  cd antiparallel
  source ../../../sh/plotCorrelation.sh
  echo "Done: z-bc = antiparallel"
  cd ../

  cd parallel
  source ../../../sh/plotCorrelation.sh
  echo "Done: z-bc = parallel"
  cd ../

  echo "Done: len_x = ${len_x}, len_z = ${len_z}."

  cd ../
done
