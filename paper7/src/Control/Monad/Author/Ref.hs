{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Author.Ref where


import           Control.Lens (use, (%=))
import qualified Control.Monad.Author.State as AS
import           Control.Monad.Author.Label (Label)
import           Control.Monad.State.Strict (MonadState(..))
import           Text.LaTeX (rendertex)
import qualified Text.LaTeX as LTX
import           Text.LaTeX.Base.Class (LaTeXC(..))
import           Data.Text (Text, pack)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

getRef :: (MonadState AS.Root m) => Label -> m Text
getRef lab = do
  map0 <- use AS.labelMap
  case Map.lookup lab map0 of
    Just str -> return str
    Nothing  -> do
      let cands :: [String]
          cands = cand0 : [s ++ [c]| s <- cands, c <- ['a'..'z']]
          cand0 = show lab

          takens :: Set.Set Text
          takens = Set.fromList $ Map.elems map0
          
          isTaken x = Set.member (pack x) takens

          freeStr :: Text
          freeStr = pack $ head $ filter (not . isTaken) cands
      AS.labelMap %= (Map.insert lab freeStr)
      return freeStr

ref :: (MonadState AS.Root m, LaTeXC (m a)) => Label -> m a
ref lab = do
  ret <- getRef lab
  LTX.ref $ rendertex ret

label :: (MonadState AS.Root m, LaTeXC (m a)) => Label -> m a
label lab = do
  ret <- getRef lab
  LTX.label $ rendertex ret
