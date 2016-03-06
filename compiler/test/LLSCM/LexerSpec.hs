{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module LLSCM.LexerSpec where

import ClassyPrelude
import Numeric (showHex)
import Data.Char (ord)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.Megaparsec

import LLSCM.Lexer
import LLSCM.Parser

main :: IO ()
main = hspec spec

exampleIdentifiers :: [(LText, String)]
exampleIdentifiers =
  [ same "..."
  , same "+soup+"
  , same "->string"
  , same "lambda"
  , same "q"
  , same "the-word-recursion-has-many-meanings"
  , same "+"
  , same "+...+"
  , same "<=?"
  , same "a34kTMNs"
  , same "list->vector"
  , same "V17a"
  , ("|two\\|words|", "two|words")
  , ("|two\\twords|", "two\twords")
  , ("|two words|", "two words")
  , ("|two\\x20;words|", "two words")
  ]
  where same a = (a, unpack a)

namedChars :: [(LText, Char)]
namedChars = [ ("#\\alarm", '\a'), ("#\\backspace", '\b'), ("#\\delete", '\x7f')
             , ("#\\escape", '\x1b'), ("#\\newline", '\n'), ("#\\null", '\0')
             , ("#\\return", '\r'), ("#\\space", ' '), ("#\\tab", '\t')
             ]

spec :: Spec
spec = do
  describe "identifier" $
    it "accepts names from specification" $
      forM_ exampleIdentifiers $ \(input, expected) ->
        parseText identifier input `shouldParse` expected

  describe "boolean" $
    it "accepts boolean values" $ do
      parseText boolean "#t" `shouldParse` True
      parseText boolean "#true" `shouldParse` True
      parseText boolean "#f" `shouldParse` False
      parseText boolean "#false" `shouldParse` False

  describe "dot" $
    it "accepts dot" $
      parseText dot "."  `shouldParse` ()

  describe "charLiteral" $ do
    prop "accepts characters" $ \x ->
      parseText charLiteral (snoc "#\\" x) `shouldParse` x

    it "accepts named chars" $
      forM_ namedChars $ \(input, expected) ->
        parseText charLiteral input `shouldParse` expected

    prop "accepts escapes" $ \x -> do
      let c = pack $ showHex (ord x) ""
      let escape = "#\\x" ++ c
      parseText charLiteral escape `shouldParse` x
