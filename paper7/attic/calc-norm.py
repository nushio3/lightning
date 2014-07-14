#!/usr/bin/env python

import pyfits

models = [['N', 'Nothing'], 
          ['JTB', '(Just TownsendBreakdown)'],
          ['JDPB', '(Just DPBreakdown)'], 
          ['JRB', '(Just RunawayBreakdown)']]


chems = ['HCOPlus', 'DCOPlus', 'N2HPlus']


dir = 'material-old/lime-output/'

for chem in chems:
  for [a1,b1] in models:
    fn1 = dir + 'LgRg16k-%s-%s-R50_100-V200.0x201.fits' % (chem , a1)
    for [a2,b2] in models:
      fn2 = dir + 'LgRg16k-%s-%s-R50_100-V200.0x201.fits' % (chem , a2)
      h1 =  pyfits.open(fn1)[0].data
      h2 =  pyfits.open(fn2)[0].data
      norm = ((h1-h2)**2).sum()
      msg = 'modelNorm %s %s %s = %e' % (chem, b1 , b2 , norm)
      print msg
