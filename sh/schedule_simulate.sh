#!/usr/bin/env bash

n_samples=10; n_sweep_therm=10; n_sweep_stead=10

cd dat/
for dir in `ls`; do
  len_z=`echo ${dir} | sed -e "s/^.*Lz\(.*\)Lx.*$/\1/"`; J=1.0
  vel=`echo ${dir} | sed -e "s/^.*_Vel\(.*\).*$/\1/"`
  len_x=`echo ${dir} | sed -e "s/^.*Lx\(.*\)_Vel.*$/\1/"`

  id_init=2; id_bound=3 #antiparallel
  source antiparallel/simulate.sh

  id_init=1; id_bound=2 #parallel
  source parallel/simulate.sh
done
cd ../
