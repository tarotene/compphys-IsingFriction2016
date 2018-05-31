#!/usr/bin/env bash

for s in $(seq 1 1 ${n_s} | printf "%.4d\n" $(cat)); do
    dir="eps_s${s}"; [ ! -e $dir ] && mkdir -p $dir
done

parallel --bar gnuplot -e \"l_t=\'${l_t}\'\; l_x=\'${l_x}\'\; l_z=\'${l_z}\'\; s=\'{2}\'\; beta=\'{1}\'\" ../../../../gp/_plotCorr_2d.gp ::: $(cat ../../list_beta.dat) ::: $(seq 1 1 ${n_s} | printf "%.4d\n" $(cat)) 
