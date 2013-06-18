-- | Parse CSS text into the AST defined in "Language.Css.Syntax".
module Language.Css.Parse where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Attoparsec.Text

import Language.Css.Syntax

-- | Parse a 'Data.Text.Text' into a 'StyleSheet' if possible.
parse :: Text -> Maybe StyleSheet
parse _ = Nothing

-- | Attoparsec parser for the CSS2.1 grammar.
styleSheetParser :: Parser StyleSheet
styleSheetParser = undefined

-- | Parse \@charset declarations.
--
-- '\@charset' $string
atcharsetp :: Parser AtCharSet
atcharsetp = undefined

-- | Parse \@import declarations.
--
-- \@import $url $media_list;
atimportp :: Parser AtImport
atimportp = undefined

-- | Parse \@media sections.
--
-- \@media $media_list { $rules }
atmediap :: Parser AtMedia
atmediap = undefined

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
-- $sel { $decls }
rulesetp :: Parser RuleSet
rulesetp = undefined

-- | Parse CSS declarations.
--
-- $property : $expression $priority?
declp :: Parser Decl
declp = undefined

-- | Parse CSS property names.
propp :: Parser Prop
propp = identp

-- | Parse CSS priority.
priop :: Parser Prio
priop = undefined

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
subselp = undefined

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
identp :: Parser Ident
identp = undefined

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
urip :: Parser Uri
urip = undefined

-- | Parse CSS string values.
stringp :: Parser String
stringp = undefined
