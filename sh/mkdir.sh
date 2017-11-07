#!/usr/bin/env bash

:>list_beta.dat
# 0. at once
# for t in `seq 0.1 0.1 2.0`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# for t in `seq 2.02 0.02 2.50`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# for t in `seq 2.6 0.1 5.0`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# 1. normally
# for t in `seq 0.1 0.1 5.0`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# 2. additionally in detail
for t in `seq 2.02 0.02 2.08`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
for t in `seq 2.12 0.02 2.18`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
for t in `seq 2.22 0.02 2.28`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
for t in `seq 2.32 0.02 2.38`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
for t in `seq 2.42 0.02 2.48`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat

for len_z in `seq -w 4 2 16`; do
  dir1="Lz${len_z}Lx`echo \"scale=0; ${len_z} * 10\" | bc`Ly__Vel10"
	dir2="Lz${len_z}Lx`echo \"scale=0; ${len_z} * 20\" | bc`Ly__Vel10"
	dir3="Lz${len_z}Lx`echo \"scale=0; ${len_z} * 30\" | bc`Ly__Vel10"
  mkdir -p dat/{${dir1},${dir2},${dir3}}/{antiparallel,parallel}

  for beta in `cat list_beta.dat`; do
    mkdir dat/{${dir1},${dir2},${dir3}}/{antiparallel,parallel}/"beta${beta}"
  done
done
