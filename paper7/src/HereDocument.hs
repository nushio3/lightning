module HereDocument(doc) where
 
import Language.Haskell.TH
import Language.Haskell.TH.Quote
 
doc :: QuasiQuoter
doc = QuasiQuoter { quoteExp = stringE }
