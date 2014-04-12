{-# LANGUAGE RecordWildCards #-}
module MachineLearning.Hopfield
    (HopfieldNet(..),
     initialize,
     initializeWith,
     activity,
     train,
     associate,
     (//),
     energy) where

import           Control.Monad
import           Control.Monad.Random hiding (fromList)
import           Data.Packed.Matrix
import           Data.Packed.ST
import           Data.Packed.Vector
import           Foreign.Storable
import           Numeric.Container

data HopfieldNet = HopfieldNet { state   :: Vector Float
                               , weights :: Matrix Float
                               } deriving (Show)


activity :: (Floating a, Ord a) => a -> a
activity activation = if activation <= 0 then -1.0 else 1.0

initialize :: Int -> HopfieldNet
initialize n = HopfieldNet (fromList (replicate n 0)) ((n >< n) (repeat 0))

initializeWith :: Matrix Float -> HopfieldNet
initializeWith patterns = train state patterns
    where
      state = initialize (cols patterns)

(//)  :: Foreign.Storable.Storable a => Vector a -> [(Int, a)] -> Vector a
vec // mutations = runSTVector $ thawVector vec >>= mutate
    where
      mutate mv = forM_ mutations (modify mv) >> return mv
      modify mv (idx, value) = modifyVector mv idx (const value)


update' :: HopfieldNet -> Int -> HopfieldNet
update' HopfieldNet{..} neuron = HopfieldNet newState weights
    where
      newState = state // [(neuron, activity activation)]
      activation = (toColumns weights !! neuron) <.> state

update :: MonadRandom m => HopfieldNet -> m HopfieldNet
update n =  liftM (update' n) $ getRandomR (0, (dim . state) n - 1)

train :: HopfieldNet -> Matrix Float -> HopfieldNet
train HopfieldNet{..}  patterns = HopfieldNet state (add weights updates)
    where
      updates = buildMatrix n n weight
      n = dim state
      scalingFactor = 1.0 / fromIntegral (rows patterns)
      weight (i, j) = (toColumns patterns !! i) <.> (toColumns patterns !! j)
                      * scalingFactor

settle
  :: (Enum b, Num b, MonadRandom m) =>
     HopfieldNet -> b -> m HopfieldNet
settle net iterations = foldM (\state _ -> update state) net [1..iterations]

associate :: MonadRandom m => HopfieldNet -> Int -> Vector Float -> m (Vector Float)
associate net iterations pattern =
    liftM state $ settle (net { state = pattern }) iterations

energy :: HopfieldNet -> Float
energy HopfieldNet{..}  = -0.5 * (mXv weights state <.> state)
