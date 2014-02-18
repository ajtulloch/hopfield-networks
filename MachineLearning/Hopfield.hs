module MachineLearning.Hopfield
    (HopfieldNet(..),
     initialize,
     initializeWith,
     activity,
     train,
     associate,
     (//),
     energy) where

import           Control.Monad        (foldM)
import           Control.Monad.Random hiding (fromList)
import           Data.Packed.Matrix
import           Data.Packed.ST
import           Data.Packed.Vector
import           Numeric.Container

data HopfieldNet = HopfieldNet { _state   :: Vector Float
                               , _weights :: Matrix Float
                               } deriving (Show)

activity :: Float -> Float
activity activation = if activation <= 0 then -1.0 else 1.0

initialize :: Int -> HopfieldNet
initialize n = HopfieldNet (fromList (replicate n 0)) ((n >< n) (repeat 0))

initializeWith :: Matrix Float -> HopfieldNet
initializeWith patterns = train state patterns
  where
    state = initialize (cols patterns)

(//) :: Vector Float -> [(Int, Float)] -> Vector Float
vec // mutations = runSTVector $
  do
    mutableVec <- thawVector vec
    mapM_ (\(idx, value) -> modifyVector mutableVec idx (const value)) mutations
    return mutableVec

update' :: HopfieldNet -> Int -> HopfieldNet
update' (HopfieldNet state weights) neuron = HopfieldNet newState weights
  where
    newState = state // [(neuron, activity activation)]
    activation = (toColumns weights !! neuron) <.> state

update :: MonadRandom m => HopfieldNet -> m HopfieldNet
update current =  do
  i <-  getRandomR (0, (dim . _state) current - 1)
  return $ update' current i


train :: HopfieldNet -> Matrix Float -> HopfieldNet
train (HopfieldNet state weights) patterns =
    HopfieldNet state (add weights updates)
  where
    updates = buildMatrix n n weight
    n = dim state
    scalingFactor = 1.0 / fromIntegral (rows patterns)
    weight (i, j) = (toColumns patterns !! i) <.> (toColumns patterns !! j) * scalingFactor

settle :: MonadRandom m => HopfieldNet -> Int -> m HopfieldNet
settle net iterations = foldM (\state _ -> update state) net [1..iterations]

associate :: MonadRandom m => HopfieldNet -> Int -> Vector Float -> m (Vector Float)
associate net iterations pattern =
    do
      settled <- settle (net { _state = pattern }) iterations
      return $ _state settled

energy :: HopfieldNet -> Float
energy (HopfieldNet state weights) = -0.5 * (mXv weights state <.> state)
