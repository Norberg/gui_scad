{-# LANGUAGE OverloadedStrings #-}
module DSL.Parser(
    parseHeader,
    parseAssignment,
    Header(..),
    skipComments,
    parseFunction,
    Name,
    Parameter
) where
import Control.Applicative
import Text.Trifecta

type Name = String
type Value = String
type Parameter = String

newtype Header = Header String
    deriving (Eq, Ord, Show)


parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'
-- these operators mean the brackets will be
-- parsed and then discarded
-- but the p will remain as our result

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

parseAssignment :: Parser (Name, Value)
parseAssignment = do
    name <- some letter
    _ <- char '='
    val <- some (noneOf "\n")
    skipEOL -- important!
    return (name, val)

parseFunction :: Parser (Name, [Parameter])
parseFunction = do
    name <- some letter
    _ <- char '('
    par <- commaSep (some (noneOf ")"))
    skipEOL
    return (name, par)

-- | Skip end of line and whitespace beyond.
skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipComments :: Parser ()
skipComments =
    skipMany (do _ <- char ';' <|> char '#'
                 skipMany (noneOf "\n")
                 skipEOL)
