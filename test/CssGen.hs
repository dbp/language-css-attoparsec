{-# LANGUAGE OverloadedStrings #-}
-- | QuickCheck generators for the Language.Css data types.
module CssGen where

import Control.Monad
import Test.QuickCheck

import Language.Css.Syntax as CSS

-- | Compromise the validity of my tests, by generating truly biased doubles.
compromiseMyself = fromInteger

newtype HtmlElement = HtmlElement String deriving (Show)

htmlElements = ["div", "span", "html", "body", "p", "ul", "li"]

instance Arbitrary HtmlElement where
  arbitrary = elements $ map HtmlElement htmlElements

instance Arbitrary Attr where
  arbitrary = do
    oneof [ liftM  Attr       arbitrary
          , liftM2 AttrIs     arbitrary arbitrary
          , liftM2 AttrIncl   arbitrary arbitrary
          , liftM2 AttrBegins arbitrary arbitrary
          ]

-- instance Arbitrary Instance where

-- instance Arbitrary Id where

instance Arbitrary AtImport where
  arbitrary = liftM2 AtImport arbitrary  arbitrary

-- @todo Should this really wrap the value in quotes?
instance Arbitrary ImportHead where
  arbitrary = oneof [ liftM (\s -> IStr $ "\"" ++ s ++ "\"") arbitrary
                    , liftM IUri arbitrary
                    ]

instance Arbitrary Value where
  arbitrary = oneof [ liftM (VDeg . Deg . compromiseMyself) arbitrary
                    , liftM (VRad . Rad . compromiseMyself) arbitrary
                    , liftM (VGrad . Grad . compromiseMyself) arbitrary
                    , liftM VColor arbitrary
                    , liftM (VHz . Hz . compromiseMyself) arbitrary
                    , liftM (VKHz . KHz . compromiseMyself) arbitrary
                    , liftM VFunc arbitrary
                    , liftM VIdent arbitrary
                    , liftM VInt arbitrary
                    , liftM (VEm . Em . compromiseMyself) arbitrary
                    , liftM (VEx . Ex . compromiseMyself) arbitrary
                    , liftM (VPx . Px) arbitrary
                    , liftM (VIn . In . compromiseMyself) arbitrary
                    , liftM (VCm . Cm . compromiseMyself) arbitrary
                    , liftM (VMm . Mm . compromiseMyself) arbitrary
                    , liftM (VPc . Pc . compromiseMyself) arbitrary
                    , liftM (VPt . Pt) arbitrary
                    , liftM (VDouble . compromiseMyself) arbitrary
                    , liftM (VPercentage . Percentage . compromiseMyself) arbitrary
                    , liftM VString arbitrary
                    , liftM (VMs . Ms . compromiseMyself) arbitrary
                    , liftM (VS . S . compromiseMyself) arbitrary
                    , liftM VUri arbitrary
                    ]

instance Arbitrary Expr where
  arbitrary = oneof [ liftM  EVal     arbitrary
                    , liftM2 SlashSep arbitrary arbitrary
                    , liftM2 CommaSep arbitrary arbitrary
                    , liftM2 SpaceSep arbitrary arbitrary
                    ]

instance Arbitrary Ident where
  arbitrary = do
    n <- choose (0,1024) :: Gen Int
    return $ Ident ("ident" ++ show n)

instance Arbitrary Color where
  arbitrary = liftM3 Crgb
    (choose (0,255))
    (choose (0,255))
    (choose (0,255))

instance Arbitrary Func where
  arbitrary = liftM2 Func arbitrary arbitrary

-- | Generate random URIs.
--
-- XXX TODO: this is completely broken.
instance Arbitrary Uri where
  arbitrary = liftM Uri arbitrary

instance Arbitrary PseudoVal where
  arbitrary =
    oneof [ liftM PIdent arbitrary -- @todo Non-empty
          , liftM PFunc arbitrary
          ]

instance Arbitrary SubSel where
  arbitrary =
    oneof [ liftM AttrSel arbitrary
          , liftM ClassSel arbitrary -- @todo Non-empty
          , liftM IdSel arbitrary -- @todo Non-empty
          , liftM PseudoSel arbitrary
          ]

instance Arbitrary SimpleSel where
  arbitrary = oneof $ map ( \f -> liftM f arbitrary)
                      (UnivSel:(map TypeSel htmlElements))

instance Arbitrary Sel where

  -- SSel SimpleSel
  -- DescendSel Sel Sel
  -- ChildSel Sel Sel
  -- AdjSel Sel Sel
  arbitrary = oneof [ liftM SSel arbitrary
                    , liftM2 DescendSel head arbitrary
                    , liftM2 ChildSel head arbitrary
                    , liftM2 AdjSel head arbitrary
                    ]
    where head = liftM SSel arbitrary
