modelNorm' 100 HCOPlus Nothing Nothing = 0.000000e+00
modelNorm' 100 HCOPlus Nothing (Just TownsendBreakdown) = 8.788762e-05
modelNorm' 100 HCOPlus Nothing (Just DPBreakdown) = 8.755116e-05
modelNorm' 100 HCOPlus Nothing (Just RunawayBreakdown) = 6.607349e-05
modelNorm' 100 HCOPlus (Just TownsendBreakdown) Nothing = 8.788762e-05
modelNorm' 100 HCOPlus (Just TownsendBreakdown) (Just TownsendBreakdown) = 0.000000e+00
modelNorm' 100 HCOPlus (Just TownsendBreakdown) (Just DPBreakdown) = 4.255395e-05
modelNorm' 100 HCOPlus (Just TownsendBreakdown) (Just RunawayBreakdown) = 5.248348e-05
modelNorm' 100 HCOPlus (Just DPBreakdown) Nothing = 8.755116e-05
modelNorm' 100 HCOPlus (Just DPBreakdown) (Just TownsendBreakdown) = 4.255395e-05
modelNorm' 100 HCOPlus (Just DPBreakdown) (Just DPBreakdown) = 0.000000e+00
modelNorm' 100 HCOPlus (Just DPBreakdown) (Just RunawayBreakdown) = 4.973951e-05
modelNorm' 100 HCOPlus (Just RunawayBreakdown) Nothing = 6.607349e-05
modelNorm' 100 HCOPlus (Just RunawayBreakdown) (Just TownsendBreakdown) = 5.248348e-05
modelNorm' 100 HCOPlus (Just RunawayBreakdown) (Just DPBreakdown) = 4.973951e-05
modelNorm' 100 HCOPlus (Just RunawayBreakdown) (Just RunawayBreakdown) = 0.000000e+00
modelNorm' 100 DCOPlus Nothing Nothing = 0.000000e+00
modelNorm' 100 DCOPlus Nothing (Just TownsendBreakdown) = 6.363311e-06
modelNorm' 100 DCOPlus Nothing (Just DPBreakdown) = 6.277919e-06
modelNorm' 100 DCOPlus Nothing (Just RunawayBreakdown) = 4.419276e-06
modelNorm' 100 DCOPlus (Just TownsendBreakdown) Nothing = 6.363311e-06
modelNorm' 100 DCOPlus (Just TownsendBreakdown) (Just TownsendBreakdown) = 0.000000e+00
modelNorm' 100 DCOPlus (Just TownsendBreakdown) (Just DPBreakdown) = 1.857057e-06
modelNorm' 100 DCOPlus (Just TownsendBreakdown) (Just RunawayBreakdown) = 2.831001e-06
modelNorm' 100 DCOPlus (Just DPBreakdown) Nothing = 6.277919e-06
modelNorm' 100 DCOPlus (Just DPBreakdown) (Just TownsendBreakdown) = 1.857057e-06
modelNorm' 100 DCOPlus (Just DPBreakdown) (Just DPBreakdown) = 0.000000e+00
modelNorm' 100 DCOPlus (Just DPBreakdown) (Just RunawayBreakdown) = 2.633752e-06
modelNorm' 100 DCOPlus (Just RunawayBreakdown) Nothing = 4.419276e-06
modelNorm' 100 DCOPlus (Just RunawayBreakdown) (Just TownsendBreakdown) = 2.831001e-06
modelNorm' 100 DCOPlus (Just RunawayBreakdown) (Just DPBreakdown) = 2.633752e-06
modelNorm' 100 DCOPlus (Just RunawayBreakdown) (Just RunawayBreakdown) = 0.000000e+00
modelNorm' 100 N2HPlus Nothing Nothing = 0.000000e+00
modelNorm' 100 N2HPlus Nothing (Just TownsendBreakdown) = 2.793545e-07
modelNorm' 100 N2HPlus Nothing (Just DPBreakdown) = 2.732830e-07
modelNorm' 100 N2HPlus Nothing (Just RunawayBreakdown) = 2.163618e-07
modelNorm' 100 N2HPlus (Just TownsendBreakdown) Nothing = 2.793545e-07
modelNorm' 100 N2HPlus (Just TownsendBreakdown) (Just TownsendBreakdown) = 0.000000e+00
modelNorm' 100 N2HPlus (Just TownsendBreakdown) (Just DPBreakdown) = 1.577490e-07
modelNorm' 100 N2HPlus (Just TownsendBreakdown) (Just RunawayBreakdown) = 1.831988e-07
modelNorm' 100 N2HPlus (Just DPBreakdown) Nothing = 2.732830e-07
modelNorm' 100 N2HPlus (Just DPBreakdown) (Just TownsendBreakdown) = 1.577490e-07
modelNorm' 100 N2HPlus (Just DPBreakdown) (Just DPBreakdown) = 0.000000e+00
modelNorm' 100 N2HPlus (Just DPBreakdown) (Just RunawayBreakdown) = 1.798235e-07
modelNorm' 100 N2HPlus (Just RunawayBreakdown) Nothing = 2.163618e-07
modelNorm' 100 N2HPlus (Just RunawayBreakdown) (Just TownsendBreakdown) = 1.831988e-07
modelNorm' 100 N2HPlus (Just RunawayBreakdown) (Just DPBreakdown) = 1.798235e-07
modelNorm' 100 N2HPlus (Just RunawayBreakdown) (Just RunawayBreakdown) = 0.000000e+00
