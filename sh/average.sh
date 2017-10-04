#!/bin/bash

# echo "n_sweep_therm, n_sweep_stead, n_samples = ?"
# read n_sweep_therm n_sweep_stead n_samples

# if [[ ! -e list_beta.dat  ]]; then
# 	for t in `seq 0.1 0.1 5.0`; do echo "scale=3; 1 / ${t}" | bc; done > list_beta.dat
# fi

# sec_elapsed=0
# n_lines=`grep -c '' list_beta.dat`
# i_line=0
for beta in `cat list_beta.dat`; do
  # i_line=`echo "scale=3; ${i_line}+1" | bc`
  # sec_i=${SECONDS}

  cp average "beta${beta}"/
  cd "beta${beta}"/
	# touch stream.dat m_z.dat spin.dat
  # echo "[average] Running: ${i_line}/${n_lines}"
  echo "${len_x} ${len_z}" | ./average
  rm average
  # rm -f stream_c*.dat m_z_c*.dat spin_c*.dat
  cp -f stream.dat ../"stream_beta${beta}.dat"
  cp -f m_z.dat ../"m_z_beta${beta}.dat"
  cp -f spin.dat ../"spin_beta${beta}.dat"
  cd ../

  # sec_f=${SECONDS}
  # sec_elapsed=`expr ${sec_elapsed} + ${sec_f} - ${sec_i}`
  # sec_estimated=`echo "scale=0; ${sec_elapsed} * (${n_lines}-${i_line}) / ${i_line}" | bc`
	#
  # hour_estimated=`expr ${sec_estimated} / 3600`
  # sec_estimated=`expr ${sec_estimated} % 3600`
  # min_estimated=`expr ${sec_estimated} / 60`
  # sec_estimated=`expr ${sec_estimated} % 60`
  # echo "[average] Done ${i_line}/${n_lines}, (Estimated ${hour_estimated}:${min_estimated}:${sec_estimated})."
done
