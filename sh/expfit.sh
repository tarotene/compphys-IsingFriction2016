# cat ../../../list_beta.dat | parallel --dryrun gnuplot -e \"min_f=\'\$\(grep min_y stats_beta{}.dat\| cut -f 2\)\'\; max_f=\'\$\(grep max_y stats_beta{}.dat\| cut -f 2\)\'\; filein=\'beta{}/stream\'\; fileout1=\'fitlog_beta{}\'\; fileout2=\'fitpic_beta{}\'\" expfit.gp

# cat ../../../list_beta.dat | parallel min_a=\$\(grep min_y \"stats_beta{}.dat\"\| cut -f 2\)\; max_a=\$\(grep max_y \"stats_beta{}.dat\"\| cut -f 2\)\; echo \"\$\{min_a\} and \$\{max_a\}\"

for beta in `cat ../../../list_beta.dat`; do
	min_y=$(grep min_y "stats_beta${beta}".dat | cut -f 2)
  max_y=$(grep max_y "stats_beta${beta}".dat | cut -f 2)
  gnuplot -e "filein='beta${beta}/stream'; fileout1='fitlog_beta${beta}'; fileout2='fitpic_beta${beta}'; min_f=${min_y}; max_f=${max_y}" expfit.gp
  echo "Done for beta = ${beta}."
done
