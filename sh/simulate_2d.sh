#!/usr/bin/env bash
#!/
#!/

if [[ ! -e ../../../list_beta.dat  ]]; then
	for t in `seq 0.1 0.1 5.0`; do echo "scale=3; 1 / ${t}" | bc; done > ../../../list_beta.dat
fi

for beta in `cat ../../../list_beta.dat`; do
  mkdir -p "beta${beta}"
done

for beta in `cat ../../../list_beta.dat`; do
	cd "beta${beta}"
  echo "${len_x} ${len_z} ${beta} ${vel} ${len_t1} ${len_t2} ${id_IC} ${id_BC} ${len_s}" | ../../../../bin/simulate_2d
	echo "Done: beta = ${beta}."
	cd ../
done
