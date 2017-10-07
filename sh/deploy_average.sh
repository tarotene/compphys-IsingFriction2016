#!/usr/bin/env bash

cd dat
for dir in `ls`; do
  cp ../average/bin/average ${dir}/antiparallel
  cp ../average/bin/average ${dir}/parallel
done
cd ../
