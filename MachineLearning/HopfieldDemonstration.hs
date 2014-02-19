{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Random     hiding (fromList)
import           Data.List.Split
import           Data.Packed.Matrix
import           Data.Packed.Vector
import           MachineLearning.Hopfield
import           Numeric.Container
import           System.Console.CmdArgs

-- Height and widght of the patterns we are training on
width, height :: Int
width = 6
height = 7

patterns :: Matrix Float
patterns = fromRows [x, o]
  where
    x = fromList
        [1, -1, -1, -1, -1, 1,
         -1, 1, -1, -1, 1, -1,
         -1, -1, 1, 1, -1, -1,
         -1, -1, 1, 1,  -1, -1,
         -1, -1, 1, 1, -1, -1,
         -1, 1, -1, -1, 1, -1,
         1, -1, -1, -1, -1, 1]
    o = fromList
        [1 , 1, 1, 1, 1, 1,
         1 , -1, -1, -1, -1, 1,
         1 , -1, -1, -1, -1, 1,
         1 , -1, -1, -1, -1, 1,
         1 , -1, -1, -1, -1, 1,
         1 , -1, -1, -1, -1, 1,
         1 , 1, 1, 1, 1, 1]

randomCorruption :: (Applicative m, MonadRandom m) => Float -> Vector Float -> m (Vector Float)
randomCorruption proportion pattern = (pattern //) <$> mutations
     where
       numMutations = (floor . (proportion *) . fromIntegral . dim) pattern
       mutationStream = zip <$> getRandomRs (0, dim pattern - 1) <*> getRandomRs (-1.0 :: Float, 1.0 :: Float)
       mutations = take numMutations . map (second activity) <$> mutationStream

-- | Gratuitously pointfree
difference :: Vector Float -> Vector Float -> Float
difference = norm2 .* sub where (.*) = (.) . (.)

validate :: HopfieldNet -> Int -> Float -> Vector Float -> IO ()
validate trained iterations corruptionLevel pattern =
    do
      corrupted <- evalRandIO $ randomCorruption corruptionLevel pattern
      reproduction <- evalRandIO $ reproduce corrupted
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

displayPattern :: Vector Float -> IO ()
displayPattern pattern =
    do
      putStrLn divider
      mapM_ printLine patternLines
      putStrLn divider
    where
      divider = replicate (width + 2) '-'
      patternLines = chunksOf width $ toList pattern
      printLine line = do
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
    eachPattern = forM_ (toRows patterns)
    validatePattern = validate trained numIterations corruptionRate
    trained = initializeWith patterns

main :: IO ()
main = cmdArgs hopfieldArgs >>= runSimulation
  where
    hopfieldArgs = HopfieldArgs {
                     _numIterations = def &= explicit &= name "num_iterations"
                   , _corruptionRate = def &= explicit &= name "corruption_rate"
                   }
