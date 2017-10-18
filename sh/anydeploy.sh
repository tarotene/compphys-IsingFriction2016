#!/usr/bin/env bash

echo "What to deploy?"
echo "(2017/10/12: {average, delete, extract, simulate} are available.)"
read deployee

cd dat
for dir in `ls`; do
  cp -f ../${deployee}/bin/${deployee} ${dir}/antiparallel
  cp -f ../${deployee}/bin/${deployee} ${dir}/parallel
done
cd ../
