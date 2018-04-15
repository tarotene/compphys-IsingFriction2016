#!/usr/bin/env bash

:>dat/Ising_2d/list_beta.dat
for t in `seq 0.10 0.10 5.00`; do
  echo "scale=4; 1/${t}" | printf "%07.4f\n" $(bc)
done >>dat/Ising_2d/list_beta.dat

for i in $(seq 1 1 7); do
  len_z=$(printf "%04g" $((2 ** ${i})))
  for j in $(seq 1 1 7); do
    len_x=$(expr ${len_z} \* 32 | printf "%04g" $(cat)); vel=$(printf "%04g" $((2 ** ${j}))) 
    parallel mkdir -p "dat/Ising_2d/Lz${len_z}Lx${len_x}Ly__Vel${vel}/{01-antiparallel,02-parallel,03-free}/beta{}/{en_step,m_step,sp_sweep,eb_sweep,ee_sweep,mb_sweep,me_sweep,p_sweep}" ::: $(cat dat/Ising_2d/list_beta.dat)
  done
done