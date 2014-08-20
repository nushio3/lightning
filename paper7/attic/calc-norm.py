#!/usr/bin/env python

import pyfits

models = [['N', 'Nothing'], 
          ['JTB', '(Just TownsendBreakdown)'],
          ['JDPB', '(Just DPBreakdown)'], 
          ['JRB', '(Just RunawayBreakdown)']]


chems = ['HCOPlus', 'DCOPlus', 'N2HPlus']


dir = 'material/lime-output/'

fp = open('src/Model/modelNorm25.hs','w')

for chem in chems:
  for [a1,b1] in models:
    fn1 = dir + 'LgAK16k-%s-%s-R25_50-V200x201.fits' % (chem , a1)
    for [a2,b2] in models:
      fn2 = dir + 'LgAK16k-%s-%s-R25_50-V200x201.fits' % (chem , a2)
      h1 =  pyfits.open(fn1)[0].data
      h2 =  pyfits.open(fn2)[0].data
      norm = ((h1-h2)**2).sum()
      msg = "modelNorm %s %s %s = %e\n" % (chem, b1 , b2 , norm)
      fp.write(msg)

fp.close()


fp = open('src/Model/modelNorm50.hs','w')

for chem in chems:
  for [a1,b1] in models:
    fn1 = dir + 'LgAK16k-%s-%s-R50_100-V200x201.fits' % (chem , a1)
    for [a2,b2] in models:
      fn2 = dir + 'LgAK16k-%s-%s-R50_100-V200x201.fits' % (chem , a2)
      h1 =  pyfits.open(fn1)[0].data
      h2 =  pyfits.open(fn2)[0].data
      norm = ((h1-h2)**2).sum()
      msg = "modelNorm %s %s %s = %e\n" % (chem, b1 , b2 , norm)
      fp.write(msg)

fp.close()
