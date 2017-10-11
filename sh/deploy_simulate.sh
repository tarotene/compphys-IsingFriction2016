#!/usr/bin/env bash

cd dat
for dir in `ls`; do
  cp -f ../simulate/bin/simulate ${dir}/antiparallel
  cp -f ../simulate/bin/simulate ${dir}/parallel
done
cd ../
