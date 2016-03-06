module LLSCM.LexerSpec where

import ClassyPrelude

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.Megaparsec

import LLSCM.Lexer
import LLSCM.Parser

main :: IO ()
main = hspec spec

reportIdentifiers :: [(LText, String)]
reportIdentifiers =
  [ same "..."
  , same "+soup+"
  , same "->string"
  , same "lambda"
  , same "q"
  , same "the-word-recursion-has-many-meanings"
  , same "+"
  , same "<=?"
  , same "a34kTMNs"
  , same "list->vector"
  , same "V17a"
  , ("|two words|", "two words")
  , ("|two\\x20;words|", "two words")
  ]
  where same a = (a, unpack a)


spec :: Spec
spec =
  describe "identifier" $ do
    it "accepts names from specification" $
      forM_ reportIdentifiers $ \(input, expected) ->
        parseText identifier input `shouldParse` expected
