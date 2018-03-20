#!/usr/bin/env bash

:>physquan.dat
for beta in `cat ../../list_beta.dat`; do
	cd "beta${beta}"
	pwd
  echo "${l_x} ${l_z} ${beta} ${vel} ${l_t} ${n_s} ${l_th} ${l_b}" | ../../../../bin/calcness_2d >> ../physquan.dat
	cd ../
done
