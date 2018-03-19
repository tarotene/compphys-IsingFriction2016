#!/usr/bin/env bash

:>physquan.dat
for beta in `cat ../../list_beta.dat`; do
	cd "beta${beta}"
  echo "${l_x} ${l_z} ${beta} ${vel} ${l_t} ${n_s} ${l_th} ${l_b}" | ../../../../bin/calcness_2d >> ../physquan.dat
	echo "Done: beta = ${beta}, len_z = ${l_z}, len_x = ${l_x}, id_BC = ${id_BC}"
	cd ../
done
