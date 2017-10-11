#!/usr/bin/env bash

touch result_beta.dat

# source: 実行時エラーを回避するお呪いなので必要!
source /opt/intel/mkl/bin/mklvars.sh intel64

# cat list_beta.dat | parallel --dry-run gnuplot -e "filename=\"stream_beta{}\"; outfilestream=\"stream_beta{}\"; len_x=\"${len_x}\"" plotStream.gp

cat list_beta.dat | parallel --dry-run gsplit m_z_beta{}.dat -a 5 -l $((${len_z}+1)) -d m_z_beta{}_split

cat list_beta.dat | parallel --dry-run gnuplot -e "filename=\"m_z_beta{}\"; n0=0; n1=$((${n_sweep_therm}+${n_sweep_stead}-1)); dn=1" plotM_z.gp

cat list_beta.dat | parallel --dry-run rm m_z_beta{}_split*
