#!/usr/bin/env bash

source /opt/intel/bin/compilervars.sh intel64
source /opt/intel/mkl/bin/mklvars.sh intel64

n_s=1
l_t=20000
l_th=4000
l_b=4000

read target

TIME_A=`date +%s`

cd dat
for dir in `ls`; do
  cd ${dir}

  l_z=`pwd | sed -e "s/^.*Lz\(.*\)Lx.*$/\1/"`
  l_x=`pwd | sed -e "s/^.*Lx\(.*\)Ly.*$/\1/"`
  l_y=`pwd | sed -e "s/^.*Ly\(.*\)_Vel.*$/\1/"`
  vel=`pwd | sed -e "s/^.*_Vel\(.*\).*$/\1/"`

  cd 01-antiparallel
  id_BC=1; id_IC=1
  source ../../../sh/${target}.sh
  cd ../

  cd 02-parallel
  id_BC=2; id_IC=2
  source ../../../sh/${target}.sh
  cd ../

  cd 03-free
  id_BC=3; id_IC=3
  source ../../../sh/${target}.sh
  cd ../

  cd ../
done
cd ../

TIME_B=`date +%s`
PT=`expr ${TIME_B} - ${TIME_A}`
H=`expr ${PT} / 3600`; PT=`expr ${PT} % 3600`; M=`expr ${PT} / 60`; S=`expr ${PT} % 60`
echo "${H}:${M}:${S}"
