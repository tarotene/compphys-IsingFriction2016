f(x)=min_f+(max_f-min_f)*exp(-(x-5000)/tau)
tau=100
FIT_LOG=fileout1."dat"
set fit quiet
fit [5001:10000] f(x) filein.".dat" using 1:4 via min_f,max_f,tau

set term postscript eps noenhanced color
set output fileout2.".eps"
plot [5001:10000] filein.".dat" u 1:4 smooth unique,f(x)
