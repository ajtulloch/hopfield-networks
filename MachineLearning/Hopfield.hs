{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module MachineLearning.Hopfield
    (HopfieldNet(..),
     initialize,
     initializeWith,
     activity,
     train,
     associate,
     (//),
     energy) where

import           Control.Applicative
import           Control.Monad
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
vec // mutations = runSTVector $ thawVector vec >>= mutate
    where
      mutate mv = forM_ mutations (modify mv) >> return mv
      modify mv (idx, value) = modifyVector mv idx (const value)

update' :: HopfieldNet -> Int -> HopfieldNet
update' (HopfieldNet state weights) neuron = HopfieldNet newState weights
    where
      newState = state // [(neuron, activity activation)]
      activation = (toColumns weights !! neuron) <.> state

update :: (Functor m, MonadRandom m) => HopfieldNet -> m HopfieldNet
update current = update' current <$> getRandomR (0, (dim . _state) current - 1)

train :: HopfieldNet -> Matrix Float -> HopfieldNet
train (HopfieldNet state weights) patterns = HopfieldNet state (add weights updates)
    where
      updates = buildMatrix n n weight
      n = dim state
      scalingFactor = 1.0 / fromIntegral (rows patterns)
      weight (i, j) = (toColumns patterns !! i) <.> (toColumns patterns !! j) * scalingFactor

settle :: (Functor m, MonadRandom m) => HopfieldNet -> Int -> m HopfieldNet
settle net iterations = foldM (\state _ -> update state) net [1..iterations]

associate :: (Functor m, MonadRandom m) => HopfieldNet -> Int -> Vector Float -> m (Vector Float)
associate net iterations pattern = liftM _state $ settle (net { _state = pattern }) iterations

energy :: HopfieldNet -> Float
energy (HopfieldNet state weights) = -0.5 * (mXv weights state <.> state)
