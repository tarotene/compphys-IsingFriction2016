#!/usr/bin/env bash

mkdir -p resdir/{antiparallel,parallel}
cp simulate/bin/simulate resdir/antiparallel/
cp simulate/bin/simulate resdir/parallel/
cp average/bin/average resdir/antiparallel/
cp average/bin/average resdir/parallel/
cp sh/simulate.sh resdir/antiparallel/
cp sh/simulate.sh resdir/parallel/
cp sh/average.sh resdir/antiparallel/
cp sh/average.sh resdir/parallel/

for len_z in `seq -w 4 2 16`; do
  dir1="Lz${len_z}Lx`echo \"scale=0; ${len_z} * 10\" | bc`_Vel1"
	dir2="Lz${len_z}Lx`echo \"scale=0; ${len_z} * 20\" | bc`_Vel1"
	dir3="Lz${len_z}Lx`echo \"scale=0; ${len_z} * 30\" | bc`_Vel1"
  mkdir dat/{${dir1}/,${dir2}/,${dir3}/}
  cp -rf resdir/ dat/${dir1}/
	cp -rf resdir/ dat/${dir2}/
	cp -rf resdir/ dat/${dir3}/
done

rm -rf resdir
