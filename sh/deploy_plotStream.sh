#!/usr/bin/env bash

cd dat
for dir in `ls`; do
  cp -f ../gp/plotStream.gp ${dir}/antiparallel
  cp -f ../gp/plotStream.gp ${dir}/parallel
done
cd ../
