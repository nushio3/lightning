{-# LANGUAGE RankNTypes #-}

module Lightning where

import           Control.Monad
import           Control.Monad.Identity (runIdentity)
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.Vector as R
import           Data.Array.Repa (Z(..), (:.)(..), (!))
import           Data.Array.Repa.Stencil 
import           Data.Array.Repa.Stencil.Dim2 
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.Random


import Field

theWidth :: Int
theHeight  :: Int
theWidth = 512
theHeight = 512


theShape :: R.DIM2
theShape = R.ix2 theHeight theWidth

initialBC :: Field R.U Int
initialBC = runIdentity $ R.computeP $ R.fromFunction theShape $ go
  where
    go (Z:.y:.x)
      | y < 16 = 1
      | y > theHeight-16 = (-1)
      | y < 150-(abs $ x - div theWidth 2) 
        && (abs $ x - div theWidth 2) < 16 = (1)
      | otherwise = 0

diffStencil :: Stencil R.DIM2 Double
diffStencil = makeStencil2 3 3 $
 (\pt -> case pt of
     (Z:.0:.0) -> Just 0.2
     (Z:.1:.0) -> Just 0.2
     (Z:.0:.1) -> Just 0.2
     (Z:.(-1):.0) -> Just 0.2
     (Z:.0:.(-1)) -> Just 0.2
     _         -> Nothing)
  


-- shift :: forall r e . (R.Source r e) => 
--          (Int, Int) -> R.Array r R.DIM2 e -> R.Array R.D R.DIM2 e
-- shift (dy,dx) = R.backpermute theShape
--       (\(Z:.y:.x) -> R.ix2 (max 0 $ min (theHeight-1) (y-dy)) (mod (x-dx) theWidth))


shrink :: (U.Unbox a)=>([a]->a) -> Field R.U a -> Field R.U a
shrink fmerge p0 = 
  runIdentity $ R.computeUnboxedP $   
  R.traverse p0 (\(Z :. ph :. pw) -> Z :. (div ph 2) :. (div pw 2))
    go
  where
    go reader (Z:.y:.x) =
      fmerge [reader $ R.ix2 (2*y) (2*x)
             ,reader $ R.ix2 (2*y+1) (2*x)
             ,reader $ R.ix2 (2*y) (2*x+1)
             ,reader $ R.ix2 (2*y+1) (2*x+1)
              ]

expand :: (U.Unbox a)=> Field R.U a -> Field R.U a
expand p0 = 
  runIdentity $ R.computeUnboxedP $     
  R.backpermute (Z :. (ph*2) :. (pw*2))
      (\(Z:.y:.x) -> (Z:.(div y 2):.(div x 2)) ) p0
  where
    (Z :. ph :. pw) = R.extent p0


diffuse :: Bool -> System -> Field R.U Double
diffuse initSw (bc0,pot0) = goes pot0
  where
    goes 
      |True = go 1 bc0 . go 2 bc0 . go 3 bc0 . go 5 bc0 . go 7 bc0 . go 8 bc0
--      |True   = go 0 bc0 .go 0 bc0 .go 0 bc0 .go 0 bc0 .go 0 bc0 .go 0 bc0 .go 0 bc0 .go 0 bc0 .go 0 bc0 .go 0 bc0 
    
    go :: Int -> Field R.U Int -> Field R.U Double -> Field R.U Double
    go n bc1 p1
      | n <=0 = diff bc1 p1
      | otherwise = 
        let bc2 = shrink shrinkBC bc1
            p2  = shrink shrinkP p1'
            p1' = diff bc1 p1
        in diff bc1 $ expand $ go (n-1) bc2 p2
           
    shrinkBC xs 
      | any (<0) xs = -1
      | any (>0) xs = 1
      | otherwise   = 0
    shrinkP xs = sum xs / fromIntegral (length xs)
      
    diff :: Field R.U Int -> Field R.U Double -> Field R.U Double 
    diff bc1 p1 = 
      runIdentity $ R.computeUnboxedP $ 
      R.zipWith sel bc1 $
      mapStencil2 BoundClamp diffStencil p1
    
    sel :: Int -> Double -> Double
    sel bc pAft
      | bc < 0 = -1
      | bc > 0 = 1
      | otherwise = pAft
                  
          
initialPotential :: Field R.U Double
initialPotential = runIdentity $ R.computeP $ R.fromFunction theShape $ go 
  where
    go (Z:.y:._) = 1+(-2)*(fromIntegral y / fromIntegral theHeight)

                    
candEdges :: Double -> System -> [(Edge,Double)]
candEdges thre (bc0, pot0) = 
  concat $
  V.foldr (:) [] $
  R.toVector $
  R.computeVectorS $
  R.fromFunction theShape go
  where
    get y x = bc0 ! (Z:.y:.x)
    go pt0@(Z:.y:.x) 
      | get y x >= 0 = []
      | otherwise    = do --list monad
        let d2 = sqrt 2
        (dy,dx,dist) <- [(0,1,1),(1,0,1),(0,-1,1),(-1,0,1)
                   ,(1,1,d2),(1,-1,d2),(-1,-1,d2),(-1,1,d2)]
        let pt1 = Z:.(y+dy):.(x+dx) 
        guard $ R.inShape theShape pt1
        guard $ get (y+dy) (x+dx) >=0
        let dPot = abs(pot0!pt1 - pot0!pt0) / dist -thre
        guard $ dPot > 0
        return ((pt0, pt1),dPot)
        
        
choose1 :: [(a,Double)] -> IO (Maybe a)
choose1 [] = return Nothing
choose1 xws = do
  let xSumWs = reverse $ snd $ foldr go (0,[]) xws
      go (x,w) (psum,accL) = (psum+w, (x,psum+w):accL)
      
      totalW = snd $ last xSumWs
  randW <- randomRIO (0,totalW)
  case dropWhile ((<randW) . snd) xSumWs of
    [] -> return Nothing
    (h:_) -> return $ Just $ fst $ h
  
chooseMany :: [(Edge,Double)] -> IO (Set.Set R.DIM2)
chooseMany xws = go Set.empty
  where
    go chosens = do             
      mayPt1 <- fmap (fmap snd) $ choose1 xws
      case mayPt1 of
        Nothing -> return chosens
        Just pt1 ->
          if pt1 `Set.member` chosens 
            then return chosens
            else go (Set.insert pt1 chosens)
             
proceed :: System -> IO System
proceed sys0@(bc0, pot0) = do
  let getDists = gd1 (1/200)
      gd1 thre = do
        cands <- chooseMany $ candEdges thre sys0
        if Set.null cands
          then gd1 (thre/2)
          else return cands
        
  dists <- getDists
  let 
    bc1 = 
      runIdentity $ R.computeUnboxedP $      
      R.traverse bc0 id go
    go reader idx
      | idx `Set.member` dists = -2
      | otherwise              = reader idx
    pot1 = diffuse False (bc1, pot0)
  return (bc1,pot1)
      
isTouchDown :: Field R.U Int -> Field R.U Int -> Bool
isTouchDown bcA bcB = 
  runIdentity $
  R.foldAllP (||) False $
  R.zipWith (\x y -> x*y<0) bcA bcB