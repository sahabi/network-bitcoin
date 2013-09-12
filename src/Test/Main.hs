module Main where

import Test.Framework ( Test, defaultMain, testGroup )
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

main :: IO ()
main = defaultMain tests 


tests :: [Test]
tests = [ testGroup "Test test" [testTest] ]


testTest :: Test
testTest = testProperty "test test" prop_test


prop_test :: Bool -> Bool
prop_test b = b || not b
