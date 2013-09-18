{-# LANGUAGE OverloadedStrings #-}
-- | QuickCheck generators for the Language.Css data types.
module CssGen where

import Control.Monad
import Test.QuickCheck

import Language.Css.Syntax as CSS

-- | Compromise the validity of my tests, by generating truly biased doubles.
compromiseMyself i = 0.3 + (fromInteger i)

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
  arbitrary = oneof [ liftM VDeg arbitrary
                    , liftM VRad arbitrary
                    , liftM VGrad arbitrary
                    , liftM VColor arbitrary
                    , liftM VHz arbitrary
                    , liftM VKHz arbitrary
                    , liftM VFunc arbitrary
                    , liftM VIdent arbitrary
                    , liftM VInt arbitrary
                    , liftM VEm arbitrary
                    , liftM VEx arbitrary
                    , liftM VPx arbitrary
                    , liftM VIn arbitrary
                    , liftM VCm arbitrary
                    , liftM VMm arbitrary
                    , liftM VPc arbitrary
                    , liftM VPt arbitrary
                    , liftM (VDouble . compromiseMyself) arbitrary
                    , liftM VPercentage arbitrary
                    , liftM VString arbitrary
                    , liftM VMs arbitrary
                    , liftM VS arbitrary
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

instance Arbitrary Deg where
  arbitrary = liftM (Deg . compromiseMyself) arbitrary

instance Arbitrary Rad where
  arbitrary = liftM (Rad . compromiseMyself) arbitrary

instance Arbitrary Grad where
  arbitrary = liftM (Grad . compromiseMyself) arbitrary

instance Arbitrary Hz where
  arbitrary = liftM (Hz . compromiseMyself) arbitrary

instance Arbitrary KHz where
  arbitrary = liftM (KHz . compromiseMyself) arbitrary

instance Arbitrary Em where
  arbitrary = liftM (Em . compromiseMyself) arbitrary

instance Arbitrary Ex where
  arbitrary = liftM (Ex . compromiseMyself) arbitrary

instance Arbitrary Px where
  arbitrary = liftM Px arbitrary

instance Arbitrary In where
  arbitrary = liftM (In . compromiseMyself) arbitrary

instance Arbitrary Cm where
  arbitrary = liftM (Cm . compromiseMyself) arbitrary

instance Arbitrary Mm where
  arbitrary = liftM (Mm . compromiseMyself) arbitrary

instance Arbitrary Pc where
  arbitrary = liftM (Pc . compromiseMyself) arbitrary

instance Arbitrary Pt where
  arbitrary = liftM Pt arbitrary

instance Arbitrary Percentage where
  arbitrary = liftM (Percentage . compromiseMyself) arbitrary

instance Arbitrary Ms where
  arbitrary = liftM (Ms . compromiseMyself) arbitrary

instance Arbitrary S where
  arbitrary = liftM (S . compromiseMyself) arbitrary
