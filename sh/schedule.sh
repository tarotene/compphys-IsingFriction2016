#!/usr/bin/env bash

n_s=2; len_t=8

read target

TIME_A=`date +%s`

cd dat
for dir in `ls`; do
  cd ${dir}

  len_z=`pwd | sed -e "s/^.*Lz\(.*\)Lx.*$/\1/"`
  len_x=`pwd | sed -e "s/^.*Lx\(.*\)Ly.*$/\1/"`
  len_y=`pwd | sed -e "s/^.*Ly\(.*\)_Vel.*$/\1/"`
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
