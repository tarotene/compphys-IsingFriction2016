#!/usr/bin/env bash

if [[ ! -e ./list_beta.dat  ]]; then
  for t in `seq 0.1 0.1 5.0`; do echo "scale=3; 1 / ${t}" | bc; done > ./list_beta.dat
fi

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

  # 下記は適宜範囲を制限
  mkdir -p dat_transfer/{${dir4},${dir5}}/{antiparallel,parallel}

  for beta in `cat list_beta.dat`; do
    mkdir dat_transfer/{${dir4},${dir5}}/{antiparallel,parallel}/"beta${beta}"
  done

  for beta in `cat list_beta.dat`; do
    cp dat/${dir4}/antiparallel/"beta${beta}"/{list_parameters.dat,stat_samples.dat,stream.dat} dat_transfer/${dir4}/antiparallel/"beta${beta}"/
    cp dat/${dir4}/parallel/"beta${beta}"/{list_parameters.dat,stat_samples.dat,stream.dat} dat_transfer/${dir4}/parallel/"beta${beta}"/

    cp dat/${dir5}/antiparallel/"beta${beta}"/{list_parameters.dat,stat_samples.dat,stream.dat} dat_transfer/${dir5}/antiparallel/"beta${beta}"/
    cp dat/${dir5}/parallel/"beta${beta}"/{list_parameters.dat,stat_samples.dat,stream.dat} dat_transfer/${dir5}/parallel/"beta${beta}"/
  done
done
