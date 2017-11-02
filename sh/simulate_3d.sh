#!/usr/bin/env bash
#!/
#!/

if [[ ! -e list_beta.dat  ]]; then
	for t in `seq 0.1 0.1 5.0`; do echo "scale=3; 1 / ${t}" | bc; done > list_beta.dat
fi

for beta in `cat list_beta.dat`; do
  mkdir -p "beta${beta}"
done

for beta in `cat list_beta.dat`; do
	cd "beta${beta}"
	touch "list_samples.dat"
  echo "${len_x} ${len_y} ${len_z} ${J} ${beta} ${vel} ${n_sweeps_therm} ${n_sweeps_stead} ${id_init} ${id_bound} ${n_samples} ${onoff_stream} ${onoff_m_z}" | ../simulate_3d
	echo "Done: beta = ${beta}."
	cd ../
done
