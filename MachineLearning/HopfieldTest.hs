module Main where

import qualified Data.Vector                          as V
import           MachineLearning.Hopfield
import           MachineLearning.Util
import           Test.Framework                       (Test, defaultMain,
                                                       testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck

instance (Arbitrary a) =>  Arbitrary (V.Vector a) where
    arbitrary = fmap V.fromList arbitrary

-- -- QuickCheck properties
prop_normPositive :: V.Vector Float -> Bool
prop_normPositive x = norm x >= 0

symmetric f x y = f y x == f x y

type VectorPredicate = (V.Vector Float -> V.Vector Float -> Bool)

prop_symmetricDotProduct :: VectorPredicate
prop_symmetricDotProduct = symmetric dotProduct

prop_symmetricDifference :: VectorPredicate
prop_symmetricDifference = symmetric difference

tests :: [Test]
tests = [
 testGroup "QuickCheck Util" [
                testProperty "norm positive" prop_normPositive,
                testProperty "dotProduct symmetric" prop_symmetricDotProduct,
                testProperty "symmetric difference" prop_symmetricDifference
               ]]

main :: IO ()
main = defaultMain tests
