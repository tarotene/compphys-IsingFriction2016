#!/usr/bin/env bash

:>list_beta.dat
for t in `seq 0.10 0.10 2.00`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
for t in `seq 2.02 0.02 2.48`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
for t in `seq 2.50 0.10 5.00`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
vel=10

len_z=(); len_x3=(); len_x4=(); len_x5=()
for i in $(seq 1 1 10); do
	len_z=("${len_z[@]}" $(printf "%04g" $((2 ** ${i}))))
done

for i in $(seq 0 1 9); do
	len_x3=("${len_x3[@]}" $(expr ${len_z[${i}]} \* 30 | printf "%05g" $(cat)))
	len_x4=("${len_x4[@]}" $(expr ${len_z[${i}]} \* 40 | printf "%05g" $(cat)))
	len_x5=("${len_x5[@]}" $(expr ${len_z[${i}]} \* 50 | printf "%05g" $(cat)))

	dir3="Lz${len_z[${i}]}Lx${len_x3[${i}]}Ly__Vel$(printf "%03g" ${vel})"
  dir4="Lz${len_z[${i}]}Lx${len_x4[${i}]}Ly__Vel$(printf "%03g" ${vel})"
  dir5="Lz${len_z[${i}]}Lx${len_x5[${i}]}Ly__Vel$(printf "%03g" ${vel})"

  mkdir -p dat/{${dir3},${dir4},${dir5}}/{01-antiparallel,02-parallel,03-free}

  for beta in $(cat list_beta.dat); do
    mkdir dat/{${dir3},${dir4},${dir5}}/{01-antiparallel,02-parallel,03-free}/"beta${beta}"
  done
done
