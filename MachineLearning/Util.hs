-- | Utility code used for matrix/vector manipulation
module MachineLearning.Util
    (norm,
     difference,
     dotProduct,
     innerProduct) where

import qualified Data.Matrix as M
import qualified Data.Vector as V

-- | The L^2 norm of a vector in R^n.
norm :: Floating a => V.Vector a -> a
norm x = sqrt $ dotProduct x x

-- | Distance between vectors in the Hilbert space induced by the L^2
-- norm on R^n.
difference :: Floating a => V.Vector a -> V.Vector a -> a
difference x y = norm (V.zipWith (-) x y)

-- | The inner product in R^n.
dotProduct :: Num b => V.Vector b -> V.Vector b -> b
dotProduct x y = V.foldl (+) 0 (V.zipWith (*) x y)

-- | The inner product on R^n induced by a PSD matrix M. Computes the
-- mapping x |-> x^T M x with x \in R^n, M \in R^{n x n}.
innerProduct :: (Num a) => M.Matrix a -> V.Vector a -> a
innerProduct m v = (M.transpose cv * m * cv) M.! (1, 1)
  where
    cv = M.colVector v
