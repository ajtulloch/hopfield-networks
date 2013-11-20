-- | Implementation of Hopfield Network training and asssociating
module MachineLearning.Hopfield
    (HopfieldNet(..),
     initializeWith,
     activity,
     train,
     associate,
     energy) where

import           Control.Monad        (foldM)
import qualified Control.Monad.Random as R
import qualified Data.Matrix          as M
import qualified Data.Vector          as V
import           MachineLearning.Util


-- | HopfieldNet maintains the state and weights of the Hopfield
-- Network, and is the major datastructure used in this code.
data HopfieldNet = HopfieldNet { _state   :: V.Vector Float
                               , _weights :: M.Matrix Float
                               } deriving (Show)

-- | Maps the activation of a neuron to the output.
activity :: Float -> Float
activity activation = if activation <= 0 then -1.0 else 1.0

initialize :: Int -> HopfieldNet
initialize n = HopfieldNet (V.replicate n 0) (M.zero n n)

-- | Initializes the HopfieldNet with the given training patterns.
initializeWith :: M.Matrix Float -> HopfieldNet
initializeWith patterns = train state patterns
  where
    state = initialize (M.ncols patterns)

update' :: HopfieldNet -> Int -> HopfieldNet
update' (HopfieldNet state weights) neuron = HopfieldNet newState weights
  where
    newState = state V.// [(neuron, activity activation)]
    -- Vector is indexed from 0, Matrix is indexed from 1.
    activation = dotProduct (M.getCol (neuron + 1) weights) state

update :: R.MonadRandom m => HopfieldNet -> m HopfieldNet
update current =  do
  i <-  R.getRandomR (0, (V.length . _state) current - 1)
  return $ update' current i

-- | Updates the weights of the Hopfield network with the given
-- training patterns.
train :: HopfieldNet -> M.Matrix Float -> HopfieldNet
train (HopfieldNet state weights) patterns =
    HopfieldNet state (weights + updates)
  where
    updates = M.matrix n n weight
    n = V.length state
    weight (i, j) = 1.0 / (fromIntegral . M.nrows) patterns *
                    dotProduct (M.getCol i patterns) (M.getCol j patterns)

settle :: R.MonadRandom m => HopfieldNet -> Int -> m HopfieldNet
settle net iterations = foldM (\state _ -> update state) net [1..iterations]

-- | Repeatedly adjusts the Hopfield network's state to minimize the
-- energy of the current configuration.
associate :: R.MonadRandom m => HopfieldNet -> Int -> V.Vector Float -> m (V.Vector Float)
associate net iterations pattern =
    do
      settled <-  settle (net { _state = pattern }) iterations
      return $ _state settled

-- | The energy of the current configuration of the Hopfield network.
energy :: HopfieldNet -> Float
energy (HopfieldNet state weights) = -0.5 * innerProduct weights state
