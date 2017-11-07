#!/usr/bin/env bash
#!/
#!/

:> result_beta.dat
for beta in `cat ../../../list_beta.dat`; do
	cd "beta${beta}"
	../delete_2d
	echo "Done: beta = ${beta}."
	cd ../
done
