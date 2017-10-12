#!/usr/bin/env bash
#!/
#!/

:> result_beta.dat
for beta in `cat list_beta.dat`; do
	cd "beta${beta}"
	# echo "${len_z} ${n_sweeps_therm} ${n_sweeps_stead} ${beta} ${begin_sweep} ${n_samples}"
	echo "${len_z} ${n_sweeps_therm} ${n_sweeps_stead} ${beta} ${n_samples}" | ../extract >> ../result_beta.dat
	echo "Done: beta = ${beta}."
	cd ../
done
