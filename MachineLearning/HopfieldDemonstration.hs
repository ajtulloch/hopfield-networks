{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import qualified Data.Matrix              as M
import qualified Data.Vector              as V

import qualified Control.Monad.Random     as R
import           Data.List.Split          (chunksOf)
import           MachineLearning.Hopfield
import           MachineLearning.Util
import           System.Console.CmdArgs   hiding (args)

-- Height and widght of the patterns we are training on
width, height :: Int
width = 6
height = 7

patterns :: M.Matrix Float
patterns = M.rowVector x M.<-> M.rowVector o
  where
    x = V.fromList
        [1, -1, -1, -1, -1, 1,
         -1, 1, -1, -1, 1, -1,
         -1, -1, 1, 1, -1, -1,
         -1, -1, 1, 1,  -1, -1,
         -1, -1, 1, 1, -1, -1,
         -1, 1, -1, -1, 1, -1,
         1, -1, -1, -1, -1, 1]
    o = V.fromList
        [1 , 1, 1, 1, 1, 1,
         1 , -1, -1, -1, -1, 1,
         1 , -1, -1, -1, -1, 1,
         1 , -1, -1, -1, -1, 1,
         1 , -1, -1, -1, -1, 1,
         1 , -1, -1, -1, -1, 1,
         1 , 1, 1, 1, 1, 1]

randomCorruption :: R.MonadRandom m => Float -> V.Vector Float -> m (V.Vector Float)
randomCorruption proportion pattern =
    do
      indices <- R.getRandomRs (0, V.length pattern - 1)
      values <-  R.getRandomRs (-1.0 :: Float, 1.0 :: Float)
      let mutatedValue = map activity values
      let mutations = take (numMutations pattern) (zip indices mutatedValue)
      return $ pattern V.// mutations
    where
      numMutations = floor . (proportion *) . fromIntegral . V.length

validate :: HopfieldNet -> Int -> Float -> V.Vector Float -> IO ()
validate trained iterations corruptionLevel pattern =
    do
      corrupted <- R.evalRandIO $ randomCorruption corruptionLevel pattern
      reproduction <- R.evalRandIO $ reproduce corrupted
      print ("Corruption error", difference corrupted pattern)
      print ("Reproduction error", difference pattern reproduction)

      print "Original"
      displayPattern pattern
      print "Corrupted"
      displayPattern corrupted
      print "Reproduction"
      displayPattern reproduction
    where
      reproduce = associate trained iterations

displayPattern :: V.Vector Float -> IO ()
displayPattern pattern =
    do
      putStrLn divider
      mapM_ printLine patternLines
      putStrLn divider
    where
      divider = replicate (width + 2) '-'
      patternLines = chunksOf width $ V.toList pattern
      printLine line =
          do
            putStr "|"
            mapM_ (putStr . repr) line
            putStrLn "|"
      repr el = if activity el <= 0 then " " else "X"

-- Command line parsing
data HopfieldArgs = HopfieldArgs { _numIterations  :: Int
                                 , _corruptionRate :: Float
                                 } deriving (Show, Data, Typeable)

runSimulation :: HopfieldArgs -> IO ()
runSimulation (HopfieldArgs numIterations corruptionRate) =
    do
      putStrLn "Training patterns"
      eachPattern displayPattern

      putStrLn "Validation"
      eachPattern validatePattern
      return ()
  where
    eachPattern f = mapM_ (\x -> f $ M.getRow x patterns) [1..M.nrows patterns]
    validatePattern = validate trained numIterations corruptionRate
    trained = initializeWith patterns

main :: IO ()
main = cmdArgs hopfieldArgs >>= runSimulation
  where
    hopfieldArgs = HopfieldArgs {
                     _numIterations = def &= explicit &= name "num_iterations"
                   , _corruptionRate = def &= explicit &= name "corruption_rate"
                   }
