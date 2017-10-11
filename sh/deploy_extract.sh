#!/usr/bin/env bash

cd dat
for dir in `ls`; do
  cp -f ../extract/bin/extract ${dir}/antiparallel
  cp -f ../extract/bin/extract ${dir}/parallel
done
cd ../
