#!/usr/bin/env bash

cd dat
for dir in `ls`; do
  cp -f ../gp/expfit.gp ${dir}/antiparallel
  cp -f ../gp/expfit.gp ${dir}/parallel
done
cd ../
