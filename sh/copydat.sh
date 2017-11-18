#!/usr/bin/env bash

if [[ ! -e ../../../list_beta.dat  ]]; then
  for t in `seq 0.1 0.1 5.0`; do echo "scale=3; 1 / ${t}" | bc; done > ../../../list_beta.dat
fi

for len_z in `seq -w 4 2 16`; do
  dir1="Lz${len_z}Lx`echo \"scale=0; ${len_z} * 10\" | bc`Ly__Vel10"
	dir2="Lz${len_z}Lx`echo \"scale=0; ${len_z} * 20\" | bc`Ly__Vel10"
	dir3="Lz${len_z}Lx`echo \"scale=0; ${len_z} * 30\" | bc`Ly__Vel10"
  mkdir -p dat_transfer/{${dir1},${dir2},${dir3}}/{antiparallel,parallel}

  for beta in `cat list_beta.dat`; do
    mkdir dat_transfer/{${dir1},${dir2},${dir3}}/{antiparallel,parallel}/"beta${beta}"
  done
done

for len_z in `seq -w 4 2 16`; do
  for beta in `cat list_beta.dat`; do
    dir1="Lz${len_z}Lx`echo \"scale=0; ${len_z} * 10\" | bc`Ly__Vel10"
    cp dat/${dir1}/antiparallel/"beta${beta}"/{list_samples.dat,list_parameters.dat,stat_samples.dat,stream.dat} dat_transfer/${dir1}/antiparallel/"beta${beta}"/
    cp dat/${dir1}/parallel/"beta${beta}"/{list_samples.dat,list_parameters.dat,stat_samples.dat,stream.dat} dat_transfer/${dir1}/parallel/"beta${beta}"/

		dir2="Lz${len_z}Lx`echo \"scale=0; ${len_z} * 20\" | bc`Ly__Vel10"
		cp dat/${dir2}/antiparallel/"beta${beta}"/{list_samples.dat,list_parameters.dat,stat_samples.dat,stream.dat} dat_transfer/${dir2}/antiparallel/"beta${beta}"/
    cp dat/${dir2}/parallel/"beta${beta}"/{list_samples.dat,list_parameters.dat,stat_samples.dat,stream.dat} dat_transfer/${dir2}/parallel/"beta${beta}"/

  	dir3="Lz${len_z}Lx`echo \"scale=0; ${len_z} * 30\" | bc`Ly__Vel10"
		cp dat/${dir3}/antiparallel/"beta${beta}"/{list_samples.dat,list_parameters.dat,stat_samples.dat,stream.dat} dat_transfer/${dir3}/antiparallel/"beta${beta}"/
    cp dat/${dir3}/parallel/"beta${beta}"/{list_samples.dat,list_parameters.dat,stat_samples.dat,stream.dat} dat_transfer/${dir3}/parallel/"beta${beta}"/
  done
done
