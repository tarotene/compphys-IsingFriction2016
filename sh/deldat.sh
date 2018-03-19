#!/usr/bin/env bash

for beta in `cat ../../../list_beta.dat`; do
	cd "beta${beta}"
  rip *
	cd ../
done