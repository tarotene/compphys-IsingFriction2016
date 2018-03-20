#!/usr/bin/env bash

set_s=$(seq 1 1 ${n_s} | printf "%.4d\n" $(cat))

cat ../../list_beta.dat | parallel --bar gnuplot -e \"beta=\'{}\'\; l_t=\'${l_t}\'\; set_s=\'${set_s}\'\" ../../../gp/_plotCorrness.gp
