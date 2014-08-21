set term postscript enhanced color solid 20
set out "output/cross-section-model.eps"
set log xy
set gri
set key bottom left
set xlabel "collision energy (eV)"
set ylabel "collision cross section (x10^{-16} cm^2)"
plot \
   'material/cross-section/H+_H2.txt'  ps 1 pt 7 lt 1 t 'H^+ H_2'  ,\
   'material/cross-section/H2+_H2.txt' ps 1 pt 7 lt 2 t 'H_2^+ H_2' ,\
   'material/cross-section/H3+_H2.txt' ps 1 pt 7 lt 3 t 'H_3^+ H_2' ,\
   'material/cross-section/N+_N2.txt'  ps 1 pt 7 lt 4 t 'N^+ N_2'  ,\
   'material/cross-section/N2+_N2.txt' ps 1 pt 7 lt 5 t 'N_2^+ N_2' ,\
   'material/cross-section/Ar+_Ar.txt' ps 1 pt 7 lt 6 t 'Ar^+ Ar' ,\
   'material/cross-section/H+_H2_model.txt'  u 1:3 w l  lt 1 t '' ,\
   'material/cross-section/H2+_H2_model.txt' u 1:3 w l  lt 2 t '' ,\
   'material/cross-section/H3+_H2_model.txt' u 1:3 w l  lt 3 t '' ,\
   'material/cross-section/N+_N2_model.txt'  u 1:3 w l  lt 4 t '' ,\
   'material/cross-section/N2+_N2_model.txt' u 1:3 w l  lt 5 t '' ,\
   'material/cross-section/Ar+_Ar_model.txt' u 1:3 w l  lt 6 t '' ,\
   'material/cross-section/HCO+_H2_model.txt'  u 1:3 w l  lt 7 lw 3 t '' ,\
   'material/cross-section/DCO+_H2_model.txt' u 1:3 w l  lt 8  lw 3 t '' ,\
   'material/cross-section/N2H+_H2_model.txt' u 1:3 w l  lt 9  lw 3 t '' ,\
   'material/cross-section/HCO+_H2_model.txt' u 1:($3*0) ps 2 pt 7 lt 7 t 'HCO^+ H_2' ,\
   'material/cross-section/DCO+_H2_model.txt' u 1:(0) ps 2 pt 7 lt 8 t 'DCO^+ H_2' ,\
   'material/cross-section/N2H+_H2_model.txt' u 1:(0) ps 2 pt 7 lt 9 t 'N_2H^^+ H_2'
