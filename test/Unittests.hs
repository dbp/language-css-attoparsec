-- | HUnit tests for the 'Language.Css.Parse' module.
module Unittests (unittests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Language.Css.Syntax
import Language.Css.Pretty
import Language.Css.Parse

import qualified Data.Text as T
import           Data.Attoparsec.Text

------------------------------------------------------------------------
-- * Unit tests

-- | All unit tests to be run.
unittests :: [Test]
unittests = []
