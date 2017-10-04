#!/usr/bin/env bash

if [[ ! -e list_beta.dat  ]]; then
  for t in `seq 0.1 0.1 5.0`; do echo "scale=3; 1 / ${t}" | bc; done > list_beta.dat
fi

for len_z in `seq -w 4 2 16`; do
  dir1="Lz${len_z}Lx`echo \"scale=0; ${len_z} * 10\" | bc`_Vel1"
	dir2="Lz${len_z}Lx`echo \"scale=0; ${len_z} * 20\" | bc`_Vel1"
	dir3="Lz${len_z}Lx`echo \"scale=0; ${len_z} * 30\" | bc`_Vel1"
  mkdir -p dat/{${dir1},${dir2},${dir3}}/{antiparallel,parallel}

  for beta in `cat list_beta.dat`; do
    mkdir dat/{${dir1},${dir2},${dir3}}/{antiparallel,parallel}/"beta${beta}"
  done
done
