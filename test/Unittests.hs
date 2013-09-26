-- | HUnit tests for the 'Language.Css.Parse' module.
module Unittests (unittests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Language.Css.Syntax
import Language.Css.Pretty
import Language.Css.Parse

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Attoparsec.Text

------------------------------------------------------------------------
-- * Unit tests

-- | All unit tests to be run.
unittests :: [Test]
unittests = [ testCase "block.css" (parseFile "block.css")
            ]

-- | Check that the package seems to do something useful when fed an input file.
parseFile :: FilePath -> IO ()
parseFile file = do
  txt <- TIO.readFile $ "test/data/" ++ file
  case (parseCss txt) of
    Just css -> putStrLn $ prettyPrint css
    Nothing -> error $ "Couldn't parse " ++ file
