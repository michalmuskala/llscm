module LLSCM.Parser
       ( parseText
       , parseFile
       )
       where

import ClassyPrelude

import Text.Megaparsec
import Text.Megaparsec.Text.Lazy

parseText :: Parser a -> LText -> Either ParseError a
parseText parser = parse parser "undefined"

parseFile :: Parser a -> String -> IO (Either ParseError a)
parseFile parser fname = do
  content <- readFile fname
  return (parse parser fname content)
