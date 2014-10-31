set term postscript enhanced color solid 30
set grid
set xlabel 'velocity (km/s)'
set ylabel 'spectral flux density (Jy)'
set xrange [-2:2]
# set log y

set out 'material/lime-output/mix-HCOPlus-V50x80-pv.eps'
plot \
  'material/lime-output/LTZS16k-HCOPlus-N-R50_100-V50x80-pv.txt' u ($1/1e3):($2) w l lw 2 t 'N', \
  'material/lime-output/LTZS16k-HCOPlus-JTB-R50_100-V50x80-pv.txt' u ($1/1e3):($2) w l lw 2 t 'T', \
  'material/lime-output/LTZS16k-HCOPlus-JDPB-R50_100-V50x80-pv.txt' u ($1/1e3):($2) w l lw 2 t 'DP', \
  'material/lime-output/LTZS16k-HCOPlus-JRB-R50_100-V50x80-pv.txt' u ($1/1e3):($2) w l lw 2 t 'R'

set out 'material/lime-output/mix-HCOPlus-V200x201-pv.eps'
plot \
  'material/lime-output/LTZS16k-HCOPlus-N-R50_100-V200x201-pv.txt' u ($1/1e3):($2) w l lw 2 t 'N', \
  'material/lime-output/LTZS16k-HCOPlus-JTB-R50_100-V200x201-pv.txt' u ($1/1e3):($2) w l lw 2 t 'T', \
  'material/lime-output/LTZS16k-HCOPlus-JDPB-R50_100-V200x201-pv.txt' u ($1/1e3):($2) w l lw 2 t 'DP', \
  'material/lime-output/LTZS16k-HCOPlus-JRB-R50_100-V200x201-pv.txt' u ($1/1e3):($2) w l lw 2 t 'R'

set out 'material/lime-output/mix-DCOPlus-V50x80-pv.eps'
plot \
  'material/lime-output/LTZS16k-DCOPlus-N-R50_100-V50x80-pv.txt' u ($1/1e3):($2) w l lw 2 t 'N', \
  'material/lime-output/LTZS16k-DCOPlus-JTB-R50_100-V50x80-pv.txt' u ($1/1e3):($2) w l lw 2 t 'T', \
  'material/lime-output/LTZS16k-DCOPlus-JDPB-R50_100-V50x80-pv.txt' u ($1/1e3):($2) w l lw 2 t 'DP', \
  'material/lime-output/LTZS16k-DCOPlus-JRB-R50_100-V50x80-pv.txt' u ($1/1e3):($2) w l lw 2 t 'R'

set out 'material/lime-output/mix-DCOPlus-V200x201-pv.eps'
plot \
  'material/lime-output/LTZS16k-DCOPlus-N-R50_100-V200x201-pv.txt' u ($1/1e3):($2) w l lw 2 t 'N', \
  'material/lime-output/LTZS16k-DCOPlus-JTB-R50_100-V200x201-pv.txt' u ($1/1e3):($2) w l lw 2 t 'T', \
  'material/lime-output/LTZS16k-DCOPlus-JDPB-R50_100-V200x201-pv.txt' u ($1/1e3):($2) w l lw 2 t 'DP', \
  'material/lime-output/LTZS16k-DCOPlus-JRB-R50_100-V200x201-pv.txt' u ($1/1e3):($2) w l lw 2 t 'R'

set out 'material/lime-output/mix-N2HPlus-V50x80-pv.eps'
plot \
  'material/lime-output/LTZS16k-N2HPlus-N-R50_100-V50x80-pv.txt' u ($1/1e3):($2) w l lw 2 t 'N', \
  'material/lime-output/LTZS16k-N2HPlus-JTB-R50_100-V50x80-pv.txt' u ($1/1e3):($2) w l lw 2 t 'T', \
  'material/lime-output/LTZS16k-N2HPlus-JDPB-R50_100-V50x80-pv.txt' u ($1/1e3):($2) w l lw 2 t 'DP', \
  'material/lime-output/LTZS16k-N2HPlus-JRB-R50_100-V50x80-pv.txt' u ($1/1e3):($2) w l lw 2 t 'R'

set out 'material/lime-output/mix-N2HPlus-V200x201-pv.eps'
plot \
  'material/lime-output/LTZS16k-N2HPlus-N-R50_100-V200x201-pv.txt' u ($1/1e3):($2) w l lw 2 t 'N', \
  'material/lime-output/LTZS16k-N2HPlus-JTB-R50_100-V200x201-pv.txt' u ($1/1e3):($2) w l lw 2 t 'T', \
  'material/lime-output/LTZS16k-N2HPlus-JDPB-R50_100-V200x201-pv.txt' u ($1/1e3):($2) w l lw 2 t 'DP', \
  'material/lime-output/LTZS16k-N2HPlus-JRB-R50_100-V200x201-pv.txt' u ($1/1e3):($2) w l lw 2 t 'R'
