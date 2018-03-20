#!/usr/bin/env bash

for beta in `cat ../../list_beta.dat`; do
	cd "beta${beta}"
	pwd
  echo "${l_x} ${l_z} ${beta} ${vel} ${l_t} ${n_s} ${l_b}" | ../../../../bin/corrness_2d
	cd ../
done
