#!/usr/bin/env bash

:>list_beta.dat
# 0. at once
for t in `seq 0.1 0.10 2.0`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
for t in `seq 2.02 0.02 2.08`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
echo "scale=3; 1 / 2.1" | bc >> list_beta.dat
for t in `seq 2.12 0.02 2.18`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
echo "scale=3; 1 / 2.2" | bc >> list_beta.dat
for t in `seq 2.22 0.02 2.28`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
echo "scale=3; 1 / 2.3" | bc >> list_beta.dat
for t in `seq 2.32 0.02 2.38`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
echo "scale=3; 1 / 2.4" | bc >> list_beta.dat
for t in `seq 2.42 0.02 2.48`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
for t in `seq 2.5 0.1 5.0`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# 0-b. at once
# for t in `seq 0.1 0.10 2.0`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# for t in `seq 2.02 0.02 2.08`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# echo "scale=3; 1 / 2.1" | bc >> list_beta.dat
# for t in `seq 2.12 0.02 2.18`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# echo "scale=3; 1 / 2.2" | bc >> list_beta.dat
# for t in `seq 2.22 0.02 2.28`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# echo "scale=3; 1 / 2.3" | bc >> list_beta.dat
# for t in `seq 2.32 0.02 2.38`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# echo "scale=3; 1 / 2.4" | bc >> list_beta.dat
# for t in `seq 2.42 0.02 2.48`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# echo "scale=3; 1 / 2.5" | bc >> list_beta.dat
# for t in `seq 2.52 0.02 2.58`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# echo "scale=3; 1 / 2.6" | bc >> list_beta.dat
# for t in `seq 2.62 0.02 2.68`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# echo "scale=3; 1 / 2.7" | bc >> list_beta.dat
# for t in `seq 2.72 0.02 2.78`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# echo "scale=3; 1 / 2.8" | bc >> list_beta.dat
# for t in `seq 2.82 0.02 2.88`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# echo "scale=3; 1 / 2.9" | bc >> list_beta.dat
# for t in `seq 2.92 0.02 2.98`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# for t in `seq 3.0 0.1 5.0`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# 1. normally
# for t in `seq 0.1 0.1 5.0`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# 2. additionally in detail
# for t in `seq 2.02 0.02 2.08`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# for t in `seq 2.12 0.02 2.18`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# for t in `seq 2.22 0.02 2.28`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# for t in `seq 2.32 0.02 2.38`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# for t in `seq 2.42 0.02 2.48`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# 2-b. additionally in more detail
# for t in `seq 2.52 0.02 2.58`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# for t in `seq 2.62 0.02 2.68`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# for t in `seq 2.72 0.02 2.78`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# for t in `seq 2.82 0.02 2.88`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat
# for t in `seq 2.92 0.02 2.98`; do echo "scale=3; 1 / ${t}" | bc; done >> list_beta.dat

for len_z in $(seq -f %03g 4 2 16); do
	len_x1=$(expr ${len_z} \* 10 | printf "%04g" $(cat))
	len_x2=$(expr ${len_z} \* 20 | printf "%04g" $(cat))
	len_x3=$(expr ${len_z} \* 30 | printf "%04g" $(cat))
  len_x4=$(expr ${len_z} \* 40 | printf "%04g" $(cat))
	len_x5=$(expr ${len_z} \* 50 | printf "%04g" $(cat))

  dir1="Lz${len_z}Lx${len_x1}Ly__Vel10"
	dir2="Lz${len_z}Lx${len_x2}Ly__Vel10"
	dir3="Lz${len_z}Lx${len_x3}Ly__Vel10"
  dir4="Lz${len_z}Lx${len_x4}Ly__Vel10"
  dir5="Lz${len_z}Lx${len_x5}Ly__Vel10"

  mkdir -p dat/{${dir1},${dir2},${dir3},${dir4},${dir5}}/{antiparallel,parallel}

  for beta in $(cat list_beta.dat); do
    mkdir dat/{${dir1},${dir2},${dir3},${dir4},${dir5}}/{antiparallel,parallel}/"beta${beta}"
  done
done
