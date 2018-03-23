#!/usr/bin/env bash

set_beta=$(cat ../../list_beta.dat)
seq 1 1 ${n_s} | printf "%.4d\n" $(cat) | parallel --bar gnuplot -e \"l_t=\'${l_t}\'\; set_s=\'{}\'; set_beta=\'${set_beta}\'\" ../../../gp/_plotCorrness.gp
