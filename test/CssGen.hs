{-# LANGUAGE OverloadedStrings #-}
-- | QuickCheck generators for the Language.Css data types.
module CssGen where

import Control.Monad
import Test.QuickCheck

import Language.Css.Syntax as CSS

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

instance Arbitrary Value where
  arbitrary = oneof [ liftM (VDeg . Deg) arbitrary
                    , liftM (VRad . Rad) arbitrary
                    , liftM (VGrad . Grad) arbitrary
                    , liftM VColor arbitrary
                    , liftM (VHz . Hz) arbitrary
                    , liftM (VKHz . KHz) arbitrary
                    , liftM VFunc arbitrary
                    , liftM VIdent arbitrary
                    , liftM VInt arbitrary
                    , liftM (VEm . Em) arbitrary
                    , liftM (VEx . Ex) arbitrary
                    , liftM (VPx . Px) arbitrary
                    , liftM (VIn . In) arbitrary
                    , liftM (VCm . Cm) arbitrary
                    , liftM (VMm . Mm) arbitrary
                    , liftM (VPc . Pc) arbitrary
                    , liftM (VPt . Pt) arbitrary
                    , liftM VDouble arbitrary
                    , liftM (VPercentage . Percentage) arbitrary
                    , liftM VString arbitrary
                    , liftM (VMs . Ms) arbitrary
                    , liftM (VS . S) arbitrary
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
  arbitrary = do
    s <- arbitrary
    return $ Uri s

instance Arbitrary PseudoVal where
  arbitrary =
    oneof [ liftM PIdent arbitrary
          , liftM PFunc arbitrary
          ]

instance Arbitrary SubSel where
  arbitrary =
    oneof [ liftM AttrSel arbitrary
          , liftM ClassSel arbitrary
          , liftM IdSel arbitrary
          , liftM PseudoSel arbitrary
          ]

instance Arbitrary SimpleSel where
  -- UnivSel [SubSel]
  -- TypeSel Element [SubSel]
  -- TODO sub selectors
  arbitrary = do
    n <- choose (0, length htmlElements) :: Gen Int
    case n of
      0 -> do
        sub <- arbitrary
        return $ UnivSel sub
      _ -> do
        sub <- arbitrary
        return $ TypeSel (htmlElements !! (n - 1)) sub

instance Arbitrary Sel where

  -- SSel SimpleSel
  -- DescendSel Sel Sel
  -- ChildSel Sel Sel
  -- AdjSel Sel Sel
  arbitrary = do
    sel <- arbitrary :: Gen SimpleSel
    return $ SSel sel
