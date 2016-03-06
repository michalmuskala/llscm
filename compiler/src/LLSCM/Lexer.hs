module LLSCM.Lexer
       ( identifier
       , stringLiteral
       , whitespace
       -- , parens
       -- , braces
       -- , brackets
       , dot
       )
       where

import ClassyPrelude

import Data.Char

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

identifier :: Parser String
identifier = lexeme identifier'
  where
    identifier'        = simpleIdentifier <|> specialIdentifier <|> peculiarIdentifier
    explicitSign       = oneOf "+-"
    subsequent         = initial <|> digitChar <|> specialSubsequent
    initial            = letterChar <|> specialInitial
    specialInitial     = oneOf "!$%&*/:< =>?^_~"
    specialSubsequent  = explicitSign <|> oneOf ".@"
    specialIdentifier  = surroundedLiteral '|'
    simpleIdentifier   = initial <:> many subsequent
    peculiarIdentifier = signPeculiar <|> dottedPeculiar
      where
        signPeculiar   = explicitSign <:> signRest
        signRest       = (signSubsequent <:> many subsequent) <|> pure []
        dottedPeculiar = middleDot <|> leadingDot
        signSubsequent = initial <|> explicitSign <|> char '@'
        middleDot      = explicitSign <:> char '.' <:> dotSubsequent <:> many subsequent
        dotSubsequent  = signSubsequent <|> char '.'
        leadingDot     = char '.' <:> dotSubsequent <:> many subsequent

stringLiteral :: Parser String
stringLiteral = lexeme (surroundedLiteral '"')

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

symbol :: String -> Parser String
symbol = L.symbol whitespace

surroundedLiteral :: Char -> Parser String
surroundedLiteral c = char c >> concat <$> manyTill charLiteral (char c)
  where
    charLiteral = unescaped <|> escape -- mnemonic <|> hexEscape
    escape      = char '\\' >> mnemonic <|> hex
    unescaped   = wrap (noneOf (c:"\\"))
    mnemonic    = choice options
      where options = map make mnemonics
            make (x, m) = (char x >> return [m]) :: Parser String
            mnemonics = [ (c, c), ('\\', '\\'), ('a', '\a'), ('b', '\b')
                        , ('t', '\t'), ('n', '\n'), ('r', '\r')]
    hex = do
      void (char 'x')
      num <- L.hexadecimal
      void (char ';')
      wrap . return . chr . fromInteger $ num

wrap :: (Applicative f) => f a -> f [a]
wrap x = (:) <$> x <*> pure []

infixr 5 <:>
(<:>) :: (Applicative f) => f a -> f [a] -> f [a]
x <:> xs = (:) <$> x <*> xs


-- whitespace :: Parser ()
-- whitespace = Token.whiteSpace lexer

-- parens :: Parser a -> Parser a
-- parens = Token.parens lexer

-- braces :: Parser a -> Parser a
-- braces = Token.braces lexer

-- brackets :: Parser a -> Parser a
-- brackets = Token.brackets lexer

