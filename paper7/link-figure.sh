mkdir -p output/figure
for i in material/lime-output/*.eps
do
	ln -s -f $(pwd)/$i  $(pwd)/output/figure/
done

ln -s -f $(pwd)/material/lime-output/mix-DCOPlus-V200x201-pv.eps $(pwd)/output/figure/mix-DCOPlus-V200x201-pv.eps     
ln -s -f $(pwd)/material/lime-output/mix-DCOPlus-V50x80-pv.eps   $(pwd)/output/figure/mix-DCOPlus-V50x80-pv.eps       
ln -s -f $(pwd)/material/lime-output/mix-HCOPlus-V200x201-pv.eps $(pwd)/output/figure/mix-HCOPlus-V200x201-pv.eps     
ln -s -f $(pwd)/material/lime-output/mix-HCOPlus-V50x80-pv.eps   $(pwd)/output/figure/mix-HCOPlus-V50x80-pv.eps       
ln -s -f $(pwd)/material/lime-output/mix-N2HPlus-V200x201-pv.eps $(pwd)/output/figure/mix-N2HPlus-V200x201-pv.eps     
ln -s -f $(pwd)/material/lime-output/mix-N2HPlus-V50x80-pv.eps   $(pwd)/output/figure/mix-N2HPlus-V50x80-pv.eps       


