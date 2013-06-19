{-# LANGUAGE OverloadedStrings #-}
-- | Parse CSS text into the AST defined in "Language.Css.Syntax".
module Language.Css.Parse where

import Prelude hiding (takeWhile)
import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Attoparsec.Combinator
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
exprp = undefined

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
subselp = choice [selip, selcp {-, selap, selpp -}]

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
selap = "[" .*> identp <*. "]"

-- | Parse CSS element names.
--
-- identp?
elementp :: Parser String
elementp = undefined

-- | Parse CSS attribute selector expressions.
attrp :: Parser Attr
attrp = undefined

-- | Parse CSS attribute names.
attridentp = identp

-- | Parse CSS pseudo-selectors.
--
-- ':' $ident
-- ':' $function
pseudovalp :: Parser PseudoVal
pseudovalp = undefined

-- | Parse CSS values.
valuep :: Parser Value
valuep = undefined

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
funcp = undefined

-- | Parse CSS angle values (degree).
degp :: Parser Deg
degp = undefined

-- | Parse CSS angle values (radian).
radp :: Parser Rad
radp = undefined

-- | Parse CSS angle values (gradian).
gradp :: Parser Grad
gradp = undefined

-- | Parse CSS color values.
colorp :: Parser Color
colorp = undefined

-- | Parse CSS frequency values (Hz).
hzp :: Parser Hz
hzp = undefined

-- | Parse CSS frequency values (kHz).
khzp :: Parser KHz
khzp = undefined

-- | Parse CSS length values (em).
emp :: Parser Em
emp = undefined

-- | Parse CSS length values (ex).
exp :: Parser Ex
exp = undefined

-- | Parse CSS length values (px).
pxp :: Parser Px
pxp = undefined

-- | Parse CSS length values (in).
inp :: Parser In
inp = undefined

-- | Parse CSS length values (cm).
cmp :: Parser Cm
cmp = undefined

-- | Parse CSS length values (mm).
mmp :: Parser Mm
mmp = undefined

-- | Parse CSS length values (pc).
--
-- XXX TODO: is this pica?
pcp :: Parser Pc
pcp = undefined

-- | Parse CSS length values (pt).
ptp :: Parser Pt
ptp = undefined

-- | Parse CSS percentage values.
percentagep :: Parser Percentage
percentagep = undefined

-- | Parse CSS time values (ms).
msp :: Parser Ms
msp = undefined

-- | Parse CSS time values (s).
sp :: Parser S
sp = undefined

-- | Parse CSS URI values.
-- url		([!#$%&*-~]|{nonascii}|{escape})*
-- "url("{w}{string}{w}")" {return URI;}
-- "url("{w}{url}{w}")"    {return URI;}
urip :: Parser Uri
urip = do
  "url(" .*> (stringp >>= return . Uri) <*. ")"
  

-- | Parse CSS string values.
--
-- XXX TODO: implement the complete grammar of quoted strings.
--
-- > string1		\"([^\n\r\f\\"]|\\{nl}|{escape})*\"
-- > string2		\'([^\n\r\f\\']|\\{nl}|{escape})*\'
-- > string		{string1}|{string2}
stringp :: Parser String
stringp = dquotesp <|> squotesp <?> "String"
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
