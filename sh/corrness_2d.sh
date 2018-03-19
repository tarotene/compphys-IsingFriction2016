#!/usr/bin/env bash

for beta in `cat ../../list_beta.dat`; do
	cd "beta${beta}"
  echo "${l_x} ${l_z} ${beta} ${vel} ${l_t} ${n_s} ${l_b}" | ../../../../bin/corrness_2d
	echo "Done: beta = ${beta}, len_z = ${l_z}, len_x = ${l_x}, id_BC = ${id_BC}"
	cd ../
done
