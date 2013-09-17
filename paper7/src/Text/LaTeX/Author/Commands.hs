{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.LaTeX.Author.Commands where

import           Control.Lens (use, (.=), (%=))
import           Control.Monad
import           Control.Monad.State (runStateT, MonadState)
import           Control.Monad.IO.Class
import           Data.Char (isAlphaNum)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Text.CSL.Input.Identifier(resolve)
import           Text.CSL.Reference (Reference)
import           Text.LaTeX as LTX
import           Text.LaTeX.Base.Syntax as LTX
import           Text.LaTeX.Base.Class as LTX
import           Text.LaTeX.Base.Commands as LTX

import           Text.LaTeX.Author.State as AS

-- | make a citation to a document(s).
cite :: (MonadState AuthorState m, MonadIO m, LaTeXC (m ())) => String -> m ()
cite url = do
  db1 <- use citationDB
  citedUrlSet %= Set.insert url
  (ref, db2) <- liftIO $ runStateT (resolve url) db1
  citationDB .= db2
  LTX.cite $ LTX.raw $ Text.pack url
  
-- | make a citation to a document(s).
citet :: (MonadState AuthorState m, MonadIO m, LaTeXC (m ())) => [String] -> m ()
citet urls = do
  forM urls $ \url -> do
    db1 <- use citationDB
    citedUrlSet %= Set.insert url
    (ref, db2) <- liftIO $ runStateT (resolve url) db1
    citationDB .= db2
  LTX.cite $ LTX.raw $ Text.intercalate "," $ map Text.pack urls


-- | refer to a label.  
ref :: (MonadState AuthorState m, LaTeXC (m a)) => AS.Label -> m a
ref lab = do
  ret <- getReference lab
  LTX.ref $ LTX.rendertex ret

-- | insert a label.
label :: (MonadState AuthorState m, LaTeXC (m a)) => AS.Label -> m a
label lab = do
  ret <- getReference lab
  LTX.label $ LTX.rendertex ret
  
getReference :: (MonadState AuthorState m) => AS.Label -> m Text
getReference lab = do
  map0 <- use labelMap
  case Map.lookup lab map0 of
    Just str -> return str
    Nothing  -> do
      let cands :: [String]
          cands = cand0 : [s ++ [c]| s <- cands, c <- ['a'..'z']]
          cand0 = filter isAlphaNum $ show lab

          takens :: Set.Set Text.Text
          takens = Set.fromList $ Map.elems map0
          
          isTaken x = Set.member (Text.pack x) takens

          freeStr :: Text.Text
          freeStr = Text.pack $ head $ filter (not . isTaken) cands
      labelMap %= (Map.insert lab freeStr)
      return freeStr
  