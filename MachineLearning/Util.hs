module MachineLearning.Util where

import qualified Data.Matrix as M
import qualified Data.Vector as V

norm :: Floating a => V.Vector a -> a
norm x = sqrt $ dotProduct x x

difference :: Floating a => V.Vector a -> V.Vector a -> a
difference x y = norm (V.zipWith (-) x y)

dotProduct :: Num b => V.Vector b -> V.Vector b -> b
dotProduct x y = V.foldl (+) 0 (V.zipWith (*) x y)

-- innerProduct M x = x^T M x.  M must be n x n, x must be of length n.
innerProduct :: (Num a) => M.Matrix a -> V.Vector a -> a
innerProduct m v = (M.transpose cv * m * cv) M.! (1, 1)
  where
    cv = M.colVector v

