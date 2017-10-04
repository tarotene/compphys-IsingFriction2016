#!/usr/bin/env bash

if [[ ! -e list_beta.dat  ]]; then
	for t in `seq 0.1 0.1 5.0`; do echo "scale=3; 1 / ${t}" | bc; done > list_beta.dat
fi

for beta in `cat list_beta.dat`; do
  mkdir -p "beta${beta}"
done

for beta in `cat list_beta.dat`; do
	cd "beta${beta}"
	touch "list_samples.dat"
  echo "${len_x} ${len_z} ${J} ${beta} ${vel} ${n_sweep_therm} ${n_sweep_stead} ${id_init} ${id_bound} ${n_samples}" | ../simulate
	cd ../
done
