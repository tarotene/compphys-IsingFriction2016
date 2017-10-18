#!/usr/bin/env bash

cd dat
for dir in `ls`; do
  cp -f ../gp/plotCorrelation.gp ${dir}/antiparallel
  cp -f ../gp/plotCorrelation.gp ${dir}/parallel
done
cd ../
