{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative  ((<*))
import           Data.Attoparsec.Text
import           Data.Either          (rights)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Language.Css.Syntax  as C

import           Language.Css.Parse
import           Test.Hspec

shouldBeRight :: (Show a, Show b) => Either a b -> Expectation
shouldBeRight = flip shouldSatisfy (\a -> case a of Right _ -> True
                                                    Left _ -> False)

shouldParse :: Show a => Parser a -> Text -> Spec
shouldParse p t = it (T.unpack t) $ shouldBeRight (parseOnly (p <* endOfInput) t)

shouldNotParse :: Show a => Parser a -> Text -> Spec
shouldNotParse p t = it (T.unpack t) (shouldBeRight . flipEither . parseOnly (p <* endOfInput) $ t)
  where flipEither (Right v) = Left v
        flipEither (Left v) = Right v

main = hspec $ do
  describe "selp" $ do
    describe "should parse" $ do
      selp `shouldParse` ".foo"
      selp `shouldParse` "* p"
      selp `shouldParse` "#id"
      selp `shouldParse` "p > a"
      selp `shouldParse` "p a"
      selp `shouldParse` "p.active"
      selp `shouldParse` "span + strong"
    describe "should not parse" $ do
      selp `shouldNotParse` "a {}"
      selp `shouldNotParse` "a ++ a"
  describe "simpleselp" $ do
    describe "should parse" $ do
      simpleselp `shouldParse` ".foo"
      simpleselp `shouldParse` "foo"
      simpleselp `shouldParse` "p.bar"
      simpleselp `shouldParse` "*.bar"
    describe "should not parse" $ do
      simpleselp `shouldNotParse` "foo + bar"
      simpleselp `shouldNotParse` "foo > bar"
  describe "rulesetp" $ do
    describe "should parse" $ do
      rulesetp `shouldParse` ".foo {}"
      rulesetp `shouldParse` "p { prop: value; }"
      rulesetp `shouldParse` "p {\n  prop: value;\n }"
      rulesetp `shouldParse` "p{prop: value;}"
      rulesetp `shouldParse` "p >q{ prop: 1em; prop-two: 2px solid #000;}"
    describe "should not parse" $ do
      rulesetp `shouldNotParse` "p"
      rulesetp `shouldNotParse` "p { prop: val prop: val }"
  describe "declp" $ do
    describe "should parse" $ do
      declp `shouldParse` "text-align: center"
      declp `shouldParse` "background-image: url(\"https://path/to/image.ext\")"
    describe "should not parse" $ do
      declp `shouldNotParse` "some prop: val"
      declp `shouldNotParse` "99hh9: val"
