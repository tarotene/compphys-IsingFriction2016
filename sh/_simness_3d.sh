#!/usr/bin/env bash

for beta in `cat ../../list_beta.dat`; do
	cd "beta${beta}"
	pwd
  echo "${l_x} ${l_y} ${l_z} ${beta} ${vel} ${l_t} ${id_IC} ${id_BC} ${n_s}" | ../../../../../bin/simness_3d
	cd ../
done
