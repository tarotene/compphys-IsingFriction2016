
#!/usr/bin/env bash

cd dat/Ising_3d
for dir in $(find . -type d -maxdepth 1 -name "Lz*Lx*Ly*Vel*" | sort); do
  cd ${dir}

  l_z=`pwd | sed -e "s/^.*Lz\(.*\)Lx.*$/\1/"`
  l_x=`pwd | sed -e "s/^.*Lx\(.*\)Ly.*$/\1/"`
  l_y=`pwd | sed -e "s/^.*Ly\(.*\)_Vel.*$/\1/"`
  vel=`pwd | sed -e "s/^.*_Vel\(.*\).*$/\1/"`

  paste -d ,  01-antiparallel/physquan1.dat 02-parallel/physquan1.dat | \
  awk -v FS=", " 'NR>1 {$14=""; $15=""; $16=""; print $1,$2,$3,($4-$17),($5+$18),($6-$19),($7+$20),($8-$21),($9+$22),($10-$23),($11+$24),($12-$25),($13+$26)}' \
  > delta_phys.dat  

  cd ../
done
cd ../../