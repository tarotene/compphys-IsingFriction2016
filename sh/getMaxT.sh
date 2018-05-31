#!/usr/bin/env bash

cd dat
: >MaxT.dat
for dir in $(find . -type d -maxdepth 1 -name "Lz*Lx*Ly*Vel*" | sort); do
    echo "${dir}, \
    $(cat ${dir}/delta_phys.dat | awk -F "," '{if(m<$12) {m=$12; n=$3}} END{print 1/n}')" \
    >>MaxT.dat
done
cd ../