#!/usr/bin/env bash

:>list_beta.dat
for t in `seq 0.10 0.10 2.00`; do echo "scale=4; 1/${t}" | printf "%07.4f\n" $(bc); done >> list_beta.dat
for t in `seq 2.02 0.02 2.48`; do echo "scale=4; 1/${t}" | printf "%07.4f\n" $(bc); done >> list_beta.dat
for t in `seq 2.50 0.10 5.00`; do echo "scale=4; 1/${t}" | printf "%07.4f\n" $(bc); done >> list_beta.dat
vel=16

len_z=(); len_x3=(); len_x4=(); len_x5=()
for i in $(seq 1 1 7); do
	len_z=("${len_z[@]}" $(printf "%04g" $((2 ** ${i}))))
done

for i in $(seq 0 1 6); do
	len_x3=("${len_x3[@]}" $(expr ${len_z[${i}]} \* 16 | printf "%04g" $(cat)))
	len_x4=("${len_x4[@]}" $(expr ${len_z[${i}]} \* 32 | printf "%04g" $(cat)))
	len_x5=("${len_x5[@]}" $(expr ${len_z[${i}]} \* 64 | printf "%04g" $(cat)))

	dir3="Lz${len_z[${i}]}Lx${len_x3[${i}]}Ly__Vel$(printf "%04g" ${vel})"
  dir4="Lz${len_z[${i}]}Lx${len_x4[${i}]}Ly__Vel$(printf "%04g" ${vel})"
  dir5="Lz${len_z[${i}]}Lx${len_x5[${i}]}Ly__Vel$(printf "%04g" ${vel})"

  mkdir -p dat/{${dir3},${dir4},${dir5}}/{01-antiparallel,02-parallel,03-free}

  for beta in $(cat list_beta.dat); do
    mkdir -p dat/{${dir3},${dir4},${dir5}}/{01-antiparallel,02-parallel,03-free}/"beta${beta}"/{en_step,m_step,sp_sweep,eb_sweep,ee_sweep,mb_sweep,me_sweep,p_sweep}
  done
done
