import           MachineLearning.Hopfield
import           Test.Framework                       (Test, defaultMain,
                                                       testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)

prop_activitySignFunction :: Float -> Bool
prop_activitySignFunction x = x == 0 || activity x == signum x

tests :: [Test]
tests = [
 testGroup "QuickCheck Hopfield" [
                testProperty "activity sign function" prop_activitySignFunction
               ]]

main :: IO ()
main = defaultMain tests
