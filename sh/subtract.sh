#!/usr/bin/env bash

cd dat
for dir in `ls`; do
  cd ${dir}
  join -t "," antiparallel/result_beta.dat parallel/result_beta.dat > result_beta.dat
  cd ../
done
cd ../
