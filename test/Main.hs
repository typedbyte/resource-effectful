module Main (main) where

import Test.Effectful.Resource (tests)
import Test.Tasty

main :: IO ()
main = defaultMain tests
