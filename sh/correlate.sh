#!/usr/bin/env bash
#!/
#!/

:> corrtime.dat
for beta in `cat list_beta.dat`; do
	cd "beta${beta}"
  ../correlate >> ../corrtime.dat
	echo "Done: beta = ${beta}."
	cd ../
done
