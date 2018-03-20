#!/usr/bin/env bash


echo "# beta, eb, err_eb, ee, err_ee, mb, err_mb, me, err_me, pump, err_pump" >physquan1.dat
echo "# beta, cb, err_cb, ce, err_ce, chib, err_chib, chie, err_chie, ub, err_ub, ue, err_ue" >physquan2.dat
for beta in `cat ../../list_beta.dat`; do
	cd "beta${beta}"
  echo "${l_x} ${l_z} ${beta} ${vel} ${l_t} ${n_s} ${l_th} ${l_b}" | ../../../../bin/calcness_2d
	cd ../
	cat "beta${beta}"/physquan1.dat >> physquan1.dat
	cat "beta${beta}"/physquan2.dat >> physquan2.dat
done
