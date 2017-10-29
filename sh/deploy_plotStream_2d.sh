#!/usr/bin/env bash

cd dat
for dir in `ls`; do
  cp -f ../gp/plotStream_2d.gp ${dir}/antiparallel
  cp -f ../gp/plotStream_2d.gp ${dir}/parallel
done
cd ../
