#!/usr/bin/env bash
#!/
#!/

for beta in `cat list_beta.dat`; do
	cd "beta${beta}"
  ../average_2d
	echo "Done: beta = ${beta}."
	cd ../
done
