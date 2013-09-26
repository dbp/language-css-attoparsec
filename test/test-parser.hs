-- | Tests for the 'Data.Hashable' module.  We test functions by
-- comparing the C and Haskell implementations.

module Main (main) where

import Properties (properties)
import Unittests (unittests)
import Test.Framework (defaultMain, testGroup)

main :: IO ()
main = defaultMain [ testGroup "unittests" unittests
                -- , testGroup "properties" properties
                   ]

