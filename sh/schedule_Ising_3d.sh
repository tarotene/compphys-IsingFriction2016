#!/usr/bin/env bash

source /opt/intel/bin/compilervars.sh intel64
source /opt/intel/mkl/bin/mklvars.sh intel64

n_s=4
l_t=20000
l_th=500
n_b=20

read target

cd dat/Ising_3d
for dir in $(find . -type d -maxdepth 1 -name "Lz*Lx*Ly*Vel*" | sort); do
  cd ${dir}

  l_z=`pwd | sed -e "s/^.*Lz\(.*\)Lx.*$/\1/"`
  l_x=`pwd | sed -e "s/^.*Lx\(.*\)Ly.*$/\1/"`
  l_y=`pwd | sed -e "s/^.*Ly\(.*\)_Vel.*$/\1/"`
  vel=`pwd | sed -e "s/^.*_Vel\(.*\).*$/\1/"`

  cd 01-antiparallel
  id_BC=1; id_IC=1
  source ../../../../sh/_${target}.sh
  cd ../

  cd 02-parallel
  id_BC=2; id_IC=2
  source ../../../../sh/_${target}.sh
  cd ../

  cd 03-free
  id_BC=3; id_IC=3
  source ../../../../sh/_${target}.sh
  cd ../

  cd ../
done
cd ../../