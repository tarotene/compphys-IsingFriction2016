#!/usr/bin/env bash

mkdir -p temp/{antiparallel,parallel}
cp simulate/bin/average temp/antiparallel/
cp simulate/bin/average temp/parallel/
cp sh/average.sh temp/antiparallel/
cp sh/average.sh temp/parallel/

cd dat/
for dir in `ls`; do
    cp -r ../temp/ ${dir}/
done
cd ../

rm -rf temp
