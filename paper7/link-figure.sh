for i in material/lime-output/*.eps
do
	ln -s -f $(pwd)/$i  $(pwd)/output/figure/
done
