#!/usr/bin/env bash

cd dat
for dir in `ls`; do
  cp -f ../gp/stats.gp ${dir}/antiparallel
  cp -f ../gp/stats.gp ${dir}/parallel
done
cd ../
