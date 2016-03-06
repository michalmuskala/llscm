{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module LLSCM.Lexer
       ( identifier
       , stringLiteral
       , whitespace
       , boolean
       , charLiteral
       -- , parens
       -- , braces
       -- , brackets
       , dot
       )
       where

import ClassyPrelude hiding (fromList)

import Data.Char
import Data.List.NonEmpty (NonEmpty(..), fromList)

import Text.Megaparsec
import Text.Megaparsec.Text.Lazy
import qualified Text.Megaparsec.Lexer as L

whitespace :: Parser ()
whitespace = L.space simpleSpace lineComment blockComment
  where simpleSpace  = void spaceChar
        lineComment  = L.skipLineComment ";"
        blockComment = L.skipBlockComment "#|" "|#"

dot :: Parser ()
dot = void (symbol ".")

boolean :: Parser Bool
boolean = lexeme "boolean" (char '#' >> true <|> false)
  where true = (char 't' <* optional (string "rue")) >> return True
        false = (char 'f' <* optional (string "alse")) >> return False

charLiteral :: Parser Char
charLiteral = lexeme "character" charLiteral'
  where charLiteral' =  string "#\\" >> hex <|> named <|> anyChar
        hex = do
          void (char 'x')
          x <- optional L.hexadecimal
          return . maybe 'x' (chr . fromInteger) $ x
        named = choice options <|> nullOrNewline
          where options = map (make . first fromList) names
                make (n:|rest, c) = do
                  void (char n)
                  rest' <- optional (string rest)
                  return (maybe n (const c) rest') :: Parser Char
                names = [ ("alarm", '\a'), ("backspace", '\b'), ("delete", '\x7f')
                        , ("escape", '\x1b'), ("return", '\r'), ("space", ' ')
                        , ("tab", '\t')
                        ]
                nullOrNewline = do
                  void (char 'n')
                  rest <- optional (nullChar <|> newlineChar)
                  return (fromMaybe 'n' rest) :: Parser Char
                nullChar = string "ull" *> pure '\0'
                newlineChar = string "ewline" *> pure '\n'

identifier :: Parser String
identifier = lexeme "identifier" identifier'
  where
    identifier'        = simpleIdentifier <|> specialIdentifier <|> peculiarIdentifier
    explicitSign       = oneOf "+-"
    subsequent         = initial <|> digitChar <|> specialSubsequent
    initial            = letterChar <|> specialInitial
    specialInitial     = oneOf "!$%&*/:< =>?^_~"
    specialSubsequent  = explicitSign <|> oneOf ".@"
    specialIdentifier  = surroundedLiteral '|'
    simpleIdentifier   = initial <:> many subsequent
    peculiarIdentifier = (withSign <|> withDot) <++> many subsequent
      where withSign = explicitSign <:> part (withDot <|> signSubsequent <:> pure "")
            withDot  = char '.' <:> dotSubsequent <:> pure ""
            signSubsequent = initial <|> explicitSign <|> char '@'
            dotSubsequent  = signSubsequent <|> char '.'
            part p         = fmap (fromMaybe "") (optional p)

stringLiteral :: Parser String
stringLiteral = lexeme "string" (surroundedLiteral '"')

lexeme :: String -> Parser a -> Parser a
lexeme name parser = L.lexeme whitespace parser <?> name

symbol :: String -> Parser String
symbol = L.symbol whitespace

surroundedLiteral :: Char -> Parser String
surroundedLiteral c = char c >> manyTill literal (char c)
  where
    literal   = unescaped <|> escape
    escape    = char '\\' >> mnemonic <|> hex
    hex       = char' 'x' >> (chr . fromInteger) <$> L.hexadecimal <* char ';'
    unescaped = noneOf (c:"\\")
    mnemonic  = choice . map make $ mnemonics
      where make (value, result) = char' value *> return result :: Parser Char
            mnemonics = [ (c, c), ('\\', '\\'), ('a', '\a'), ('b', '\b')
                        , ('t', '\t'), ('n', '\n'), ('r', '\r')
                        ]

infixr 5 <:>
(<:>) :: (Applicative f) => f a -> f [a] -> f [a]
x <:> xs = (:) <$> x <*> xs

infixr 5 <++>
(<++>) :: (Applicative f) => f [a] -> f [a] -> f [a]
lhs <++> rhs = (++) <$> lhs <*> rhs


-- whitespace :: Parser ()
-- whitespace = Token.whiteSpace lexer

-- parens :: Parser a -> Parser a
-- parens = Token.parens lexer

-- braces :: Parser a -> Parser a
-- braces = Token.braces lexer

-- brackets :: Parser a -> Parser a
-- brackets = Token.brackets lexer

