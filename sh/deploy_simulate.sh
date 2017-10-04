#!/usr/bin/env bash

mkdir -p temp/{antiparallel,parallel}
cp simulate/bin/simulate temp/antiparallel/
cp simulate/bin/simulate temp/parallel/
cp sh/simulate.sh temp/antiparallel/
cp sh/simulate.sh temp/parallel/

cp -rf temp/ dat/**/
rm -rf temp
