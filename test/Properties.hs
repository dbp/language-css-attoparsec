-- | QuickCheck tests for the 'Language.Css.Parse' module.
module Properties (properties) where

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
roundTrip pprint parser input = case (parseOnly parser $ pprint input) of
  Left  _ -> False
  Right p -> p == input

-- | Property: (print . parse) == id for 'Sel'.
roundtripImport :: AtImport -> Bool
roundtripImport = roundTrip (T.pack . prettyPrint) atimportp

-- | Property: (print . parse) == id for 'Sel'.
roundtripSelector :: Sel -> Bool
roundtripSelector = roundTrip (T.pack . prettyPrint) selp

-- | Property: (print . parse) == id for 'Value'.
roundtripValue :: Value -> Bool
roundtripValue = roundTrip (T.pack . prettyPrint) valuep

-- | Property: (print . parse) == id for 'Color'.
roundtripColor :: Color -> Bool
roundtripColor = roundTrip (T.pack . prettyPrint) colorp

-- | Property: (print . parse) == id for 'Uri'.
roundtripUri :: Uri -> Bool
roundtripUri = roundTrip (T.pack . prettyPrint) urip

-- | All properties to be tested.
properties :: [Test]
properties =
    [ testGroup "roundtrip"
      [ testProperty "roundtrip/selector" roundtripSelector
      , testProperty "roundtrip/import" roundtripImport
      , testProperty "roundtrip/color" roundtripColor
      , testProperty "roundtrip/uri" roundtripUri
      , testProperty "roundtrip/value" roundtripValue
      ]
    ]
