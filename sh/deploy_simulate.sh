#!/usr/bin/env bash

cd dat
for dir in `ls`; do
  cp ../simulate/bin/simulate ${dir}/antiparallel
  cp ../simulate/bin/simulate ${dir}/parallel
done
cd ../
