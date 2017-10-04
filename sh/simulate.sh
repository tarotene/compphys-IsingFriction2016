#!/usr/bin/env bash

if [[ ! -e list_beta.dat  ]]; then
	for t in `seq 0.1 0.1 5.0`; do echo "scale=3; 1 / ${t}" | bc; done > list_beta.dat
fi

for beta in `cat list_beta.dat`; do
  mkdir -p "beta${beta}"
done

source /opt/intel/mkl/bin/mklvars.sh intel64 mod ilp64

sec_elapsed=0
n_lines=`grep -c '' list_beta.dat`
i_line=0
for beta in `cat list_beta.dat`; do
  i_line=`echo "scale=3; ${i_line}+1" | bc`
  sec_i=${SECONDS}

  cp simulate "beta${beta}"/
  cd "beta${beta}"/
  touch "list_clone.dat"
  echo "[simulate] Running ${i_line}/${n_lines}, (Param: ${len_x}, ${len_z}, ${J}, ${beta}, ${vel}, ${n_sweep_therm}, ${n_sweep_stead}, ${id_init}, ${id_bound}, ${n_clones})."
  echo "${len_x} ${len_z} ${J} ${beta} ${vel} ${n_sweep_therm} ${n_sweep_stead} ${id_init} ${id_bound} ${n_clones}" | ./simulate
  rm simulate
  cd ../

  sec_f=${SECONDS}
  sec_elapsed=`expr ${sec_elapsed} + ${sec_f} - ${sec_i}`
  sec_estimated=`echo "scale=0; ${sec_elapsed} * (${n_lines}-${i_line}) / ${i_line}" | bc`

  hour_estimated=`expr ${sec_estimated} / 3600`
  sec_estimated=`expr ${sec_estimated} % 3600`
  min_estimated=`expr ${sec_estimated} / 60`
  sec_estimated=`expr ${sec_estimated} % 60`
  echo "[simulate] Done ${i_line}/${n_lines}, (Estimated:  ${hour_estimated}:${min_estimated}:${sec_estimated})."
done
