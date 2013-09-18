-- | QuickCheck tests for the 'Language.Css.Parse' module.
module Properties (properties) where

import           Prelude hiding (exp)

import qualified Data.Text as T
import           Data.Attoparsec.Text
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)

import           Language.Css.Syntax
import           Language.Css.Pretty
import           Language.Css.Parse

import           CssGen

--------------------------------------------------------------------------------
-- * Properties

-- | Round-trip property between an attoparsec parser and a pretty printer.
roundTrip parser input = case (parseOnly parser $ pprint input) of
  Left  _ -> False
  Right p -> p == input
  where pprint = (T.pack . prettyPrint)

-- | All properties to be tested.
properties :: [Test]
properties =
    [ testGroup "roundTrip"
      [ testProperty "selector" $ roundTrip selp
      , testProperty "import" $ roundTrip atimportp
      , testProperty "color" $ roundTrip colorp
      , testProperty "uri" $ roundTrip urip
      , testProperty "value" $ roundTrip valuep
      ]
    , testGroup "values"
      [ testProperty "hz" $ roundTrip hzp
      , testProperty "degree" $ roundTrip degreep
      , testProperty "radian" $ roundTrip radianp
      , testProperty "gradian" $ roundTrip gradianp
      , testProperty "khz" $ roundTrip khzp
      , testProperty "em" $ roundTrip emp
      , testProperty "ex" $ roundTrip exp
      , testProperty "px" $ roundTrip pxp
      , testProperty "in" $ roundTrip inp
      , testProperty "cm" $ roundTrip cmp
      , testProperty "mm" $ roundTrip mmp
      ]
    ]
