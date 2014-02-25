#!/usr/bin/env python
import pyfits
import math
import subprocess
hdulist0 = pyfits.open('material/lime-output/LgRg16k-HCOPlus-N-R50_100-V50.0x80.fits')
hdulist2 = pyfits.open('material/lime-output/LgRg16k-HCOPlus-JRB-R50_100-V50.0x80.fits')
img0 = hdulist0[0].data
img2 = hdulist2[0].data
nv = 80
perBeam = 3.14*0.65*0.44/0.025**2
velRes = 0.05
irange = [24,32,36,40,44,48,56]
for i in irange:
    fp_g = open('material/lime-output/2d.gnuplot','w')
    gnuplot_script = """
set term postscript enhanced color 30
set grid
set pm3d
set pm3d map
set size ratio -1
set cbrange [0:1]
set xrange[-300:300]
set yrange[-300:300]
set palette defined (  0 1 1 1, 0.1 0.5 0.5 0.5  , 1 0 0 0 )
set xtics nomirror rotate by 90
{setytics}
{setcb}
set title 'v={dopplerVel}km/s'
set out 'material/lime-output/2d-r-{index}.eps'
splot 'material/lime-output/2d.txt' t ''
"""
    v = velRes * (i-40)

    print >> fp_g, gnuplot_script.format(index=str(i), setytics=('' if (i==irange[0]) else 'set format y ""'), setcb=('' if (i==irange[-1]) else 'unset colorbox'), dopplerVel = str(v))
    fp_g.close()

    fp = open('material/lime-output/2d.txt','w')
    for x in range(img0.shape[2]):
        for y in range(img0.shape[1]):
            if (x%2==0 and y%2==0):
                ax = (200-x) * 0.025 * 56
                ay = (y-200) * 0.025 * 56
                avg4 = (img2[i,y,x] + img2[i,y+1,x] + img2[i,y+1,x+1] + img2[i,y,x+1])/4.0
                print >> fp, ax, ay, avg4 * perBeam
        if (x%2==0):
            print >> fp, ''    
    fp.close
    subprocess.call('gnuplot material/lime-output/2d.gnuplot', shell=True)

