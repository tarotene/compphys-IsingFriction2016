#!/usr/bin/env bash

touch result_beta.dat

cat list_beta.dat | parallel --bar gnuplot -e \"filein=\'beta{}/stream\'\; fileout=\'stream_beta{}\'\; len_x=\'${len_x}\'\; len_z=\'${len_z}\'\; vel=\'${vel}\'\" plotStream_2d.gp

# cat list_beta.dat | parallel --dry-run gsplit m_z_beta{}.dat -a 5 -l $((${len_z}+1)) -d m_z_beta{}_split
#
# cat list_beta.dat | parallel --dry-run gnuplot -e "filename=\"m_z_beta{}\"; n0=0; n1=$((${n_sweep_therm}+${n_sweep_stead}-1)); dn=1" plotM_z.gp
#
# cat list_beta.dat | parallel --dry-run rm m_z_beta{}_split*
