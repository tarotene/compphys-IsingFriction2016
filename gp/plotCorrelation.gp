set grid
set term postscript eps color

set output "autocorrelation_th_diss0.eps"
plot for [i_sample=1:n_samples] sprintf("autocorrelation_th_s%04d.dat", i_sample) u 1:2 smooth unique notitle
set output "autocorrelation_th_energy0.eps"
plot for [i_sample=1:n_samples] sprintf("autocorrelation_th_s%04d.dat", i_sample) u 1:3 smooth unique notitle

set output "autocorrelation_st_pump0.eps"
plot for [i_sample=1:n_samples] sprintf("autocorrelation_st_s%04d.dat", i_sample) u 1:2 smooth unique notitle
set output "autocorrelation_st_diss0.eps"
plot for [i_sample=1:n_samples] sprintf("autocorrelation_st_s%04d.dat", i_sample) u 1:3 smooth unique notitle
set output "autocorrelation_st_energy0.eps"
plot for [i_sample=1:n_samples] sprintf("autocorrelation_st_s%04d.dat", i_sample) u 1:4 smooth unique notitle

set output "autocorrelation_th_diss.eps"
plot "autocorrelation_th.dat" u 1:2 smooth unique notitle
set output "autocorrelation_th_energy.eps"
plot "autocorrelation_th.dat" u 1:3 smooth unique notitle

set output "autocorrelation_st_pump.eps"
plot "autocorrelation_st.dat" u 1:2 smooth unique notitle
set output "autocorrelation_st_diss.eps"
plot "autocorrelation_st.dat" u 1:3 smooth unique notitle
set output "autocorrelation_st_energy.eps"
plot "autocorrelation_st.dat" u 1:4 smooth unique notitle
