#!/usr/bin/env bash

n_samples=2; n_sweeps_therm=20; n_sweeps_stead=20
waste=2000; size_block=200

echo "What to schedule?"
echo "(2018/02/02: {average_2d, extract_2d, simulate_2d, plotStream_2d, stats, expfit} are available.)"
read scheduled

cd dat
for dir in `ls`; do
  cd ${dir}

  len_z=`pwd | sed -e "s/^.*Lz\(.*\)Lx.*$/\1/"`
  len_x=`pwd | sed -e "s/^.*Lx\(.*\)Ly.*$/\1/"`
  len_y=`pwd | sed -e "s/^.*Ly\(.*\)_Vel.*$/\1/"`
  vel=`pwd | sed -e "s/^.*_Vel\(.*\).*$/\1/"`

  cd 01-antiparallel
  id_bound=1
  id_init=1
  source ../../../sh/${scheduled}.sh
  echo "Done: len_z = ${len_z}, len_x = ${len_x}, len_y = ${len_y}, z-bc = antiparallel"
  cd ../

  cd 02-parallel
  id_bound=2
  id_init=2
  source ../../../sh/${scheduled}.sh
  echo "Done: len_z = ${len_z}, len_x = ${len_x}, len_y = ${len_y}, z-bc = parallel"
  cd ../

  cd 03-free
  id_bound=3
  id_init=3
  source ../../../sh/${scheduled}.sh
  echo "Done: len_z = ${len_z}, len_x = ${len_x}, len_y = ${len_y}, z-bc = free"
  cd ../

  cd ../
done
cd ../
