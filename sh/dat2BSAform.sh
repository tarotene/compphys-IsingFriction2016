#!/usr/bin/env bash

:>BSAinput.dat
echo "# L T A Error of A" >> BSAinput.dat
for len_z in `seq -w 8 2 16`; do
	dir="Lz${len_z}Lx`echo \"scale=0; ${len_z} * 30\" | bc`Ly__Vel10"
  # cat dat_20171106/${dir}/result_beta.dat | awk -v lz="${len_z}" 'BEGIN {FS=", ";OFS=", "} {print lz, 1/$1, $2-$8, $3+$9}' >> BSAinput.dat
	cat dat_20171106/${dir}/result_beta.dat | awk -v lz="${len_z}" '{print lz, 1/$1, $2-$8, $3+$9}' >> BSAinput.dat
done
