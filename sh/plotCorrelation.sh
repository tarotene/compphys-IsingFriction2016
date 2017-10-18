#!/usr/bin/env bash
#!/
#!/

for beta in `cat list_beta.dat`; do
	cd "beta${beta}"
  gnuplot -e "n_samples = ${n_samples}" ../plotCorrelation.gp
	echo "Done: beta = ${beta}."
	cd ../
done
