#!/usr/bin/env bash

for beta in `cat ../../../list_beta.dat`; do
	cd "beta${beta}"
	# echo "${l_x} ${l_z} ${beta} ${vel} ${l_t} ${id_IC} ${id_BC} ${n_s}"
  echo "${l_x} ${l_z} ${beta} ${vel} ${l_t} ${id_IC} ${id_BC} ${n_s}" | ../../../../bin/simness_2d
	echo "Done: beta = ${beta}, len_z = ${l_z}, len_x = ${l_x}, id_BC = ${id_BC}"
	cd ../
done
