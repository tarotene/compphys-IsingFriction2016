#!/usr/bin/env bash

echo "#  l_z,     beta,  vel,       eb,    err_eb,       mb,   err_mb">physquan1.dat
echo "#  l_z,     beta,  vel,       cb,    err_cb,     chib, err_chib,       ub,   err_ub">physquan2.dat
for beta in `cat ../../list_beta.dat`; do
	cd "beta${beta}"
  echo "${l_x} ${l_y} ${l_z} ${beta} ${l_t} ${n_s} ${l_th} ${n_b}" | ../../../../../bin/calc_3d_eq
	cd ../
	cat "beta${beta}"/physquan1.dat >> physquan1.dat
	cat "beta${beta}"/physquan2.dat >> physquan2.dat
done
