#!/usr/bin/env bash
#!/
#!/

for beta in `cat list_beta.dat`; do
	cd "beta${beta}"
  echo "${len_x} ${len_z}" | ../average
	echo "Done: beta = ${beta}."
	cd ../
done
