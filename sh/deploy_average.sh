#!/usr/bin/env bash

cd dat
for dir in `ls`; do
  cp -f ../average/bin/average ${dir}/antiparallel
  cp -f ../average/bin/average ${dir}/parallel
done
cd ../
