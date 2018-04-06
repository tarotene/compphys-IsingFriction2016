#!/usr/bin/env bash

for beta in `cat ../../list_beta.dat`; do
	cd "beta${beta}"
	pwd
    # echo "${l_x} ${l_z} ${beta} ${l_t} ${id_IC} ${id_BC} ${n_s}" | ../../../../../bin/Ising_2d_eq
    echo "${l_x} ${l_z} ${beta} ${l_t} ${id_IC} ${id_BC} ${n_s}"
	cd ../
done
