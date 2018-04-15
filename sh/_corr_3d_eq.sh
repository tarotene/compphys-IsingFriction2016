#!/usr/bin/env bash

for beta in `cat ../../list_beta.dat`; do
	cd "beta${beta}"
	pwd
  echo "${l_x} ${l_y} ${l_z} ${beta} ${l_t} ${n_s} ${l_b}" | ../../../../../bin/corr_3d_eq
	cd ../
done
