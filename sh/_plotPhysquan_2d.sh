#!/usr/bin/env bash

BC=`pwd | sed -e "s/^.*-\(.*\).*$/\1/"`
file="physquan_${BC}"

gnuplot -e "file='${file}'" ../../../../gp/_plotPhysquan.gp