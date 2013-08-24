{-# LANGUAGE OverloadedStrings #-}
-- | Parse CSS text into the AST defined in "Language.Css.Syntax".
module Language.Css.Parse where

import Prelude hiding (takeWhile, exp)
import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Attoparsec.Combinator
import           Data.Bits (shiftL)
import           Data.Char
import           Data.Int
import           Data.Text (Text)
import qualified Data.Text as T

import           Language.Css.Syntax

-- | Parse a 'Data.Text.Text' into a 'StyleSheet' if possible.
parseCss :: Text -> Maybe StyleSheet
parseCss _ = Nothing

-- | Attoparsec parser for the CSS2.1 grammar.
styleSheetParser :: Parser StyleSheet
styleSheetParser = undefined

-- | Parse a CSS \@charset rule.
--
-- > @charset "UTF-8";
atcharsetp :: Parser AtCharSet
atcharsetp = do
  string "@charset"
  skipSpace
  name <- stringp
  skipSpace
  char ';'
  return $ AtCharSet name

-- | Parse a CSS \@import rule.
--
-- > @import "hello.css";
-- > @import url("//example.com/code.css") mobile;
atimportp :: Parser AtImport
atimportp = do
  string "@import"
  skipSpace
  head <- atimportheadp
  -- let head = IStr "Hello"
  skipSpace
  media <- mediap `sepBy` (string ",")
  -- let media = []
  skipSpace
  char ';'
  return $ AtImport head media

-- | Parse the location of an \@import statment.
-- 
-- This parser accepts either a quoted string or a CSS url literal.
atimportheadp :: Parser ImportHead
atimportheadp = (stringp >>= return . IStr)
                <|> (urip >>= return . IUri)
                <?> "Import head"

-- | Parse \@media sections.
--
-- > @media tv, screen { ... }
atmediap :: Parser AtMedia
atmediap = do
  media <- identp `sepBy` char ','
  let rules = [] -- XXX TODO
  return $ AtMedia media rules

-- | Parse \@page rules.
--
-- \@page $ident? $pseudopage? { $decls }
atpagep :: Parser AtPage
atpagep = undefined

-- | Parse \@font-face declarations.
--
-- \@font-face { $decls }
atfontfacep :: Parser AtFontFace
atfontfacep = undefined

-- | Parse CSS rulesets.
--
-- > $sel { $decls }
rulesetp :: Parser RuleSet
rulesetp = undefined

-- | Parse CSS declarations.
--
-- > $property ':' $expression $priority? ';'?
declp :: Parser Decl
declp = do
  name <- propp
  skipSpace
  char ':'
  skipSpace
  val <- exprp
  skipSpace
  pri <- priop
  skipSpace
  char ';'
  skipSpace
  return $ Decl pri name val

-- | Parse CSS property names.
propp :: Parser Prop
propp = identp

-- | Parse CSS priority.
--
-- XXX TODO Check this for space, etc.
--
-- > !important
priop :: Parser (Maybe Prio)
priop = (string "!important" >> return (Just Important)) <|> (return Nothing)

-- | Parse CSS value expressions.
exprp :: Parser Expr
exprp = EVal <$> valuep

-- | Parse CSS selectors.
selp :: Parser Sel
selp = undefined

-- | Parse simple CSS selectors.
--
-- '*' $subsel*
-- $el $subsel*
simpleselp :: Parser SimpleSel
simpleselp = undefined

-- | Parse CSS sub-selectors.
--
-- attribute selectors
-- class selectors
-- id selectors
-- pseudo selectors
subselp :: Parser SubSel
subselp = choice [selip, selcp , selap {-, selpp-}]

-- XXX TODO: This is extremely wrong.
selip = char '#' >> takeWhile1 (not . isHorizontalSpace)
        >>= return . IdSel . T.unpack

-- XXX TODO: This is extremely wrong.
selcp = char '.' >> takeWhile1 (not . isHorizontalSpace)
        >>= return . ClassSel . T.unpack

-- XXX TODO: This is very wrong.
selpp = do
  char ':'
  s <- identp
  return $ PseudoSel 

-- XXX TODO: This is very wrong.
selap = do
  char '['
  a <- attrp
  char ']'
  return $ (AttrSel a)

-- | Parse CSS element names.
--
-- identp?
elementp :: Parser String
elementp = undefined

-- | Parse CSS attribute selector expressions.
attrp :: Parser Attr
attrp =
  char '[' *> {- space* -} (choice options) {- space* -} <* char ']'
  <?> "Attribute selector."
  where
    options = [attr_is, attr_contains, attr_starts, attr_name]
    attr_name = (Attr) <$> attridentp <?> "Attribute present selector."
    attr_is = do
      n <- attridentp
      -- space*
      char '='
      -- space*
      v <- attridentp
      return $ AttrIs n v
      <?> "Attribute value selector."
    attr_contains = fail "None" <?> "Attribute 'contains' selector."
    attr_starts = fail "None" <?> "Attribute 'starts' selector."

-- | Parse CSS attribute names.
attridentp = many1 (letter <|> digit)

-- | Parse CSS pseudo-selectors.
--
-- ':' $ident
-- ':' $function
pseudovalp :: Parser PseudoVal
pseudovalp = undefined

-- | Parse CSS values.
valuep :: Parser Value
valuep = choice [ VDeg <$> degreep
                , VRad <$> radianp
                , VGrad <$> gradianp
                , VColor <$> colorp
                , VHz <$> hzp
                , VKHz <$> khzp
                , VPercentage <$> percentagep
                , VEm <$> emp
                , VEx <$> exp
                , VPx <$> pxp
                , VIn <$> inp
                , VCm <$> cmp
                , VMm <$> mmp
                , VPc <$> pcp
                , VPt <$> ptp
                , VString <$> stringp
                , VMs <$> msp
                , VS <$> sp
                , VFunc <$> funcp
                , VUri <$> urip
                --, VIdent <$> identp
                , VDouble <$> signed double
                , VInt <$> signed decimal
                ]

-- | Parse CSS identifiers.
--
-- nmstart		[_a-z]|{nonascii}|{escape}
-- nmchar		[_a-z0-9-]|{nonascii}|{escape}
-- ident		-?{nmstart}{nmchar}*
identp :: Parser Ident
identp = do
  dash <- option "" (string "-")
  first <- satisfy nmstart
  rest <- takeWhile nmchar
  return $ Ident $ (T.unpack dash) ++ first:(T.unpack rest)

-- | Parse CSS media values.
mediap :: Parser Ident
mediap = identp

-- | Parse CSS functions.
--
-- $ident '(' $expr ')'
funcp :: Parser Func
funcp = Func <$> identp <*> (char '(' *> exprp <* char ')')

-- | Parse a 'degrees' value.
--
-- > 1.1deg
degreep = Deg <$> (double <* string "deg")

-- | Parse a 'radians' value.
--
-- > 1.1rad
radianp = Rad <$> (double <* string "rad")

-- | Parse a 'gradians' value.
--
-- > 1.1grad
gradianp = Grad <$> (double <* string "grad")

-- | Parse a color value.
--
-- > #fff
-- > #f0f0f0
-- > rgb(128,255,0)
-- > fuchsia
colorp :: Parser Color
colorp = colorname
         <|> hex6color
         <|> hex3color
         <|> rgbcolor
         <?> "Color value"

-- | Parse color name values.
colorname :: Parser Color
colorname = Cword . T.unpack <$> (choice $ map string $ [ "aqua", "black", "blue", "fuchsia", "gray"
                                  , "green", "lime", "maroon", "navy", "olive"
                                  , "orange", "purple", "red", "silver", "teal"
                                  , "white", "yellow"])
            <?> "Color name"

-- | Parse 6-digit hexadecimal color values.
hex6color = char '#' *> (Crgb <$> hex <*> hex <*> hex) <?> "6-digit hexadecimal color value."
  where
    hex :: Parser Int
    hex = intFromHex <$> satisfy isHexDigit <*> satisfy isHexDigit

-- | Parse 3-digit hexadecimal color values.
hex3color = char '#' *> (Crgb <$> hex <*> hex <*> hex) <?> "3-digit hexadecimcal color value."
  where
    hex :: Parser Int
    hex = (\i -> intFromHex i i) <$> satisfy isHexDigit

-- | Parse RGB color value.
--
-- NB: This parser accepts values outside the range of 0..255. This is
-- deliberate and required by the specification.
-- 
-- > rgb(1,255,78)
-- > rgb(110%,0,50%)
--
-- TODO: This parser should allow whitespace internally.
rgbcolor = string "rgb(" *> ( Crgb <$> val <* char ',' <*> val <* char ',' <*> val) <* char ')'
  where
    val  = (pval <|> nval)
    pval = round . (2.55 *) . fromInteger <$> (skipSpace *> signed decimal <* char '%' <* skipSpace)
    nval = skipSpace *> (signed decimal) <* skipSpace

-- | Parse CSS frequency values (Hz).
--
-- > 10.1hz
--
-- TODO: Should be case insensitive
hzp :: Parser Hz
hzp = Hz <$> (signed double) <* asciiCI "hz"

-- | Parse CSS frequency values (kHz).
--
-- > 10.1khz
--
-- TODO: Should be case insensitive
khzp :: Parser KHz
khzp = KHz <$> (signed double) <* asciiCI "khz"

-- | Parse CSS length values (em).
--
-- > 10em
--
-- TODO: Should be case insensitive.
emp :: Parser Em
emp = Em <$> (signed double) <* string "em"

-- | Parse CSS length values (ex).
exp :: Parser Ex
exp = Ex <$> (signed double) <* string "ex"

-- | Parse CSS length values (px).
pxp :: Parser Px
pxp = Px <$> (signed decimal) <* string "px"

-- | Parse CSS length values (in).
inp :: Parser In
inp = In <$> (signed double) <* string "in"

-- | Parse CSS length values (cm).
cmp :: Parser Cm
cmp = Cm <$> (signed double) <* string "cm"

-- | Parse CSS length values (mm).
mmp :: Parser Mm
mmp = Mm <$> (signed double) <* string "mm"

-- | Parse CSS length values (pc).
--
-- XXX TODO: is this pica?
pcp :: Parser Pc
pcp = Pc <$> (signed double) <* string "pc"

-- | Parse CSS length values (pt).
ptp :: Parser Pt
ptp = Pt <$> (signed decimal) <* string "pt"

-- | Parse CSS percentage values.
percentagep :: Parser Percentage
percentagep = Percentage <$> (signed double) <* char '%'

-- | Parse CSS time values (ms).
msp :: Parser Ms
msp = Ms <$> (signed double) <* string "ms"

-- | Parse CSS time values (s).
sp :: Parser S
sp = S <$> (signed double) <* string "s"

-- | Parse CSS URI values.
--
-- "url("{w}{string}{w}")" {return URI;}
urip :: Parser Uri
urip = Uri <$> (asciiCI "url(" *> skipSpace *> (stringp <|> urlp) <* skipSpace <* char ')')

-- | Parse an un-quoted URL in CSS.
--
-- TODO: This should accept ([!#$%&*-~]|{nonascii}|{escape})*
urlp :: Parser String
urlp = T.unpack <$> takeTill (== ')') <?> "Unquoted URL string."

-- | Parse CSS string values.
--
-- XXX TODO: implement the complete grammar of quoted strings.
--
-- > string1		\"([^\n\r\f\\"]|\\{nl}|{escape})*\"
-- > string2		\'([^\n\r\f\\']|\\{nl}|{escape})*\'
-- > string		{string1}|{string2}
stringp :: Parser String
stringp = dquotesp <|> squotesp <?> "Quoted string."
  where dquotesp = "\"" .*> stringOf (/= '"') <*. "\""
        squotesp = "'" .*> stringOf (/= '\'') <*. "'"
        stringOf p = takeWhile p >>= return . T.unpack

-- | Parse escaped unicode characters.
--
-- XXX TODO This should only accept {1,6}.
unicodep :: Parser Char
unicodep = do
  char '\\'
  hexadecimal >>= return . toEnum

-- | Parse new line escaped new line characters within CSS string values.
--
-- nl		\n|\r\n|\r|\f
nl :: Parser Char
nl = choice [char '\n', string "\r\n" >> return '\n', char '\r', char '\f']

-- | CSS nonterminal nmstart
nmstart	:: Char -> Bool
nmstart = flip elem $ "[_a-z]|{nonascii}|{escape}"

nmchar :: Char -> Bool
nmchar = flip elem $ "[_a-z0-9-]|{nonascii}|{escape}"

-- * Utilities

-- | Convert two hex digits to an int.
--
-- Assumes that the characters are valid hexadecimal digits.
intFromHex :: Char -> Char -> Int
intFromHex h l = (high $ ord h) + (low $ ord l)
  where 
    high w | w >= 48 && w <= 57  = fromIntegral (w - 48) `shiftL` 4
           | w >= 97             = fromIntegral (w - 87) `shiftL` 4
           | otherwise           = fromIntegral (w - 55) `shiftL` 4
    low w | w >= 48 && w <= 57  = fromIntegral (w - 48)
          | w >= 97             = fromIntegral (w - 87)
          | otherwise           = fromIntegral (w - 55)
