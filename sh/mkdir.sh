#!/usr/bin/env bash

:>list_beta.dat
for t in `seq 0.10 0.10 5.00`; do echo "scale=4; 1/${t}" | printf "%07.4f\n" $(bc); done >> list_beta.dat
vel=16

len_z=(); len_z=("${len_z[@]}" $(printf "%04g" $((2 ** ${i}))))

len_x=()
for i in $(seq 0 1 6); do
	len_x=("${len_x[@]}" $(expr ${len_z[${i}]} \* 32 | printf "%04g" $(cat)))

	dir="Lz${len_z[${i}]}Lx${len_x[${i}]}Ly__Vel$(printf "%04g" ${vel})"
  mkdir -p dat/{${dir}}/{01-antiparallel,02-parallel,03-free}

  for beta in $(cat list_beta.dat); do
    mkdir -p dat/{${dir}}/{01-antiparallel,02-parallel,03-free}/"beta${beta}"/{en_step,m_step,sp_sweep,eb_sweep,ee_sweep,mb_sweep,me_sweep,p_sweep}
  done
done
