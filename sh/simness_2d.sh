#!/usr/bin/env bash

for beta in `cat ../../../list_beta.dat`; do
	cd "beta${beta}"
  echo "${len_x} ${len_z} ${beta} ${vel} ${len_t} ${id_IC} ${id_BC} ${n_s}" | ../../../../bin/simness_2d
	echo "Done: beta = ${beta}, len_z = ${len_z}, len_x = ${len_x}, id_BC = ${id_BC}"
	cd ../
done
