#!/usr/bin/env bash

mkdir -p temp/{antiparallel,parallel}
cp simulate/bin/simulate temp/antiparallel/
cp simulate/bin/simulate temp/parallel/
cp sh/simulate.sh temp/antiparallel/
cp sh/simulate.sh temp/parallel/

cd dat/
for dir in `ls`; do
    cp -r ../temp/ ${dir}/
done
cd ../

rm -rf temp
