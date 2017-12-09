#!/usr/bin/env bash

# 生データの切り出し
len_x_vel=$(expr ${len_x} \* ${vel}); volume=$(expr ${len_x} \* ${len_z})
awk -v lxv="${len_x_vel}" '{printf "%-15s %-15s %-15s\n", 1/$1, $2/lxv, $3/lxv}' result_beta.dat > res_pump_T.dat
awk -v lxv="${len_x_vel}" '{printf "%-15s %-15s %-15s\n", 1/$1, $4/lxv, $5/lxv}' result_beta.dat > res_diss_T.dat
awk -v vol="${volume}" '{printf "%-15s %-15s %-15s\n", 1/$1, $6/vol, $7/vol}' result_beta.dat > res_energy_T.dat

# 微分
awk 'NR==1{lastx=0; lasty=0} NR!=1{dy=$2-lasty; dx=$1-lastx; lasty=$2; lastx=$1; printf "%-15s %-15s\n", $1-dx*0.5, dy/dx}' res_pump_T.dat > res_dPump_T.dat
awk 'NR==1{lastx=0; lasty=0} NR!=1{dy=$2-lasty; dx=$1-lastx; lasty=$2; lastx=$1; printf "%-15s %-15s\n", $1-dx*0.5, dy/dx}' res_diss_T.dat > res_dDiss_T.dat
awk 'NR==1{lastx=0; lasty=0} NR!=1{dy=$2-lasty; dx=$1-lastx; lasty=$2; lastx=$1; printf "%-15s %-15s\n", $1-dx*0.5, dy/dx}' res_energy_T.dat > res_dEnergy_T.dat
