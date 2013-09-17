language-css-attoparsec
=======================

This package contains [attoparsec][1] parsers for the [language-css][2] library.

[1]: http://hackage.haskell.org/package/attoparsec
[2]: http://hackage.haskell.org/package/language-css

[![Build Status](https://travis-ci.org/thsutton/language-css-attoparsec.png?branch=master)](https://travis-ci.org/thsutton/language-css-attoparsec)

Caveats
-------

This package is not complete and almost certainly won't do anything you find
useful, even in part.

This package does not meet all of the requirements described in the CSS
specification. In particular, it does not implement error handling and
recovery.

This is not a *compliant parser* but a parser for *compliant documents*.
