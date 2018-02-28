#!/usr/bin/env bash

len_s=4; len_t1=32; len_t2=32
waste=2000; size_block=200

echo "What to schedule?"
echo "(2018/02/02: {average_2d, extract_2d, simulate_2d, plotStream_2d, stats, expfit} are available.)"
read scheduled

TIME_A=`date +%s`

cd dat
for dir in `ls`; do
  cd ${dir}

  len_z=`pwd | sed -e "s/^.*Lz\(.*\)Lx.*$/\1/"`
  len_x=`pwd | sed -e "s/^.*Lx\(.*\)Ly.*$/\1/"`
  len_y=`pwd | sed -e "s/^.*Ly\(.*\)_Vel.*$/\1/"`
  vel=`pwd | sed -e "s/^.*_Vel\(.*\).*$/\1/"`

  # cd 01-antiparallel
  # id_BC=1; id_IC=1
  # source ../../../sh/${scheduled}.sh
  # echo "Done: len_z = ${len_z}, len_x = ${len_x}, len_y = ${len_y}, z-bc = antiparallel"
  # cd ../
  #
  # cd 02-parallel
  # id_BC=2; id_IC=2
  # source ../../../sh/${scheduled}.sh
  # echo "Done: len_z = ${len_z}, len_x = ${len_x}, len_y = ${len_y}, z-bc = parallel"
  # cd ../

  cd 03-free
  id_BC=3; id_IC=3
  source ../../../sh/${scheduled}.sh
  echo "Done: len_z = ${len_z}, len_x = ${len_x}, len_y = ${len_y}, z-bc = free"
  cd ../

  cd ../
done
cd ../

TIME_B=`date +%s`
PT=`expr ${TIME_B} - ${TIME_A}`
H=`expr ${PT} / 3600`; PT=`expr ${PT} % 3600`; M=`expr ${PT} / 60`; S=`expr ${PT} % 60`
echo "${H}:${M}:${S}"
