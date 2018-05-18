#!/usr/bin/env bash

for i in $(seq 1 1 7); do
  len_z=$(printf "%04g" $((2 ** ${i})))
  # for j in $(seq 1 1 7); do
    len_x1=$(expr ${len_z} \* 32 | printf "%04g" $(cat))
    len_x2=$(expr ${len_z} \* 64 | printf "%04g" $(cat))
    # vel=$(printf "%04g" $((2 ** ${j})))
    vel="0010"
    mkdir -p dat/Ising_2d/Lz${len_z}Lx${len_x1}Ly__Vel${vel}/{01-antiparallel,02-parallel,03-free}/
    mkdir -p dat/Ising_2d/Lz${len_z}Lx${len_x2}Ly__Vel${vel}/{01-antiparallel,02-parallel,03-free}/
    :>dat/Ising_2d/Lz${len_z}Lx${len_x1}Ly__Vel${vel}/list_beta.dat
    :>dat/Ising_2d/Lz${len_z}Lx${len_x2}Ly__Vel${vel}/list_beta.dat
    for t in `seq 0.10 0.10 5.00`; do
    echo "scale=4; 1/${t}" | printf "%07.4f\n" $(bc)
    done >>dat/Ising_2d/Lz${len_z}Lx${len_x1}Ly__Vel${vel}/list_beta.dat
    for t in `seq 0.10 0.10 5.00`; do
    echo "scale=4; 1/${t}" | printf "%07.4f\n" $(bc)
    done >>dat/Ising_2d/Lz${len_z}Lx${len_x2}Ly__Vel${vel}/list_beta.dat
  # done
done
