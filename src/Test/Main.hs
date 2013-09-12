module Main where

import Test.Framework ( defaultMain )
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.API
import Test.QuickCheck

main :: IO ()
main = defaultMain $ tests ++ testsIO


tests :: [Test]
tests = [ testGroup "Test test" [testTest] ]


testsIO :: [Test]
testsIO = map buildTest [ ioTest ]


testTest :: Test
testTest = testProperty "Test" prop_test


ioTest :: IO Test
ioTest = do
    putStrLn "Making an IO test."
    return $ testProperty "IO Test" prop_test 


prop_test :: Bool -> Bool
prop_test b = b || not b


