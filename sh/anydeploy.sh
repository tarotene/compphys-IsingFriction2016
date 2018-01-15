#!/usr/bin/env bash

echo "What to deploy?"
echo "(2017/10/28: {average_2d, extract_2d, simulate_2d, plotStream_2d} are available.)"
read deployee

cd dat
for dir in `ls`; do
  cp -f ../bin/${deployee} ${dir}/01-antiparallel
  cp -f ../bin/${deployee} ${dir}/02-parallel
  cp -f ../bin/${deployee} ${dir}/03-free
done
cd ../
