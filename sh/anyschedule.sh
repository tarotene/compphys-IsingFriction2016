#!/usr/bin/env bash

J=1.0; n_samples=50; n_sweeps_therm=200; n_sweeps_stead=200
onoff_stream=1; onoff_m_z=0
waste=500; size_block=500

echo "What to schedule?"
echo "(2017/10/28: {average_2d, extract_2d, simulate_2d, plotStream_2d} are available.)"
read scheduled

cd dat
for dir in `ls`; do
  cd ${dir}

  len_z=`pwd | sed -e "s/^.*Lz\(.*\)Lx.*$/\1/"`
  len_x=`pwd | sed -e "s/^.*Lx\(.*\)Ly.*$/\1/"`
  len_y=`pwd | sed -e "s/^.*Ly\(.*\)_Vel.*$/\1/"`
  vel=`pwd | sed -e "s/^.*_Vel\(.*\).*$/\1/"`

  cd antiparallel
  id_init=2
  id_bound=1
  source ../../../sh/${scheduled}.sh
  echo "Done: len_z = ${len_z}, len_x = ${len_x}, len_y = ${len_y}, z-bc = antiparallel"
  cd ../

  cd parallel
  id_init=1
  id_bound=2
  source ../../../sh/${scheduled}.sh
  echo "Done: len_z = ${len_z}, len_x = ${len_x}, len_y = ${len_y}, z-bc = parallel"
  cd ../

  cd ../
done
cd ../
