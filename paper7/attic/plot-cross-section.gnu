set term postscript enhanced color  20
set out "output/cross-section-model.eps"
set yrange [0.5:1000]
set log xy
set gri
set key bottom left
set xlabel "collision energy (eV)"
set ylabel "collision cross section (x10^{-16} cm^2)"
plot \
   'material/cross-section/H+_H2.txt'  ps 1 pt 1 lt 1 t 'H^+ H_2'  ,\
   'material/cross-section/H2+_H2.txt' ps 1 pt 2 lt 2 t 'H_2^+ H_2' ,\
   'material/cross-section/H3+_H2.txt' ps 1 pt 3 lt 3 t 'H_3^+ H_2' ,\
   'material/cross-section/N+_N2.txt'  ps 1 pt 4 lt 4 t 'N^+ N_2'  ,\
   'material/cross-section/N2+_N2.txt' ps 1 pt 5 lt 5 t 'N_2^+ N_2' ,\
   'material/cross-section/Ar+_Ar.txt' ps 1 pt 6 lt 6 lc rgb "#c0c080" t 'Ar^+ Ar' ,\
   'material/cross-section/H+_H2_model.txt'  u 1:3 w l  lt 1 t '' ,\
   'material/cross-section/H2+_H2_model.txt' u 1:3 w l  lt 2 t '' ,\
   'material/cross-section/H3+_H2_model.txt' u 1:3 w l  lt 3 t '' ,\
   'material/cross-section/N+_N2_model.txt'  u 1:3 w l  lt 4 t '' ,\
   'material/cross-section/N2+_N2_model.txt' u 1:3 w l  lt 5 t '' ,\
   'material/cross-section/Ar+_Ar_model.txt' u 1:3 w l  lt 6  lc rgb "#c0c080" t '' ,\
   'material/cross-section/HCO+_H2_model.txt'  u 1:3 w l lt 1  lw 5  lc rgb "#000000" t '' ,\
   'material/cross-section/DCO+_H2_model.txt' u 1:3 w l  lt 2  lw 5  lc rgb "#f08000" t '' ,\
   'material/cross-section/N2H+_H2_model.txt' u 1:3 w l  lt 3  lw 5  lc rgb "#808080" t '' ,\
   'material/cross-section/HCO+_H2_model.txt' u 1:($3*0) ps 2 pt 7 lt 7 lc rgb "#000000" t 'HCO^+ H_2' ,\
   'material/cross-section/DCO+_H2_model.txt' u 1:(0)    ps 2 pt 7 lt 8 lc rgb "#f08000" t 'DCO^+ H_2' ,\
   'material/cross-section/N2H+_H2_model.txt' u 1:(0)    ps 2 pt 7 lt 9 lc rgb "#808080" t 'N_2H^^+ H_2'
