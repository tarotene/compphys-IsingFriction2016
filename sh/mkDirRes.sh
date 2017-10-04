#!/usr/bin/env bash

for len_z in `seq -w 4 2 16`; do
  dir1="Lz${len_z}Lx`echo \"scale=0; ${len_z} * 10\" | bc`_Vel1"
	dir2="Lz${len_z}Lx`echo \"scale=0; ${len_z} * 20\" | bc`_Vel1"
	dir3="Lz${len_z}Lx`echo \"scale=0; ${len_z} * 30\" | bc`_Vel1"
  mkdir dat/{${dir1}/,${dir2}/,${dir3}/}
done
