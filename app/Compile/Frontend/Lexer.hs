module Compile.Frontend.Lexer
  ( sc
  , symbol
  , parens
  , braces
  , semi
  , identifier
  , numberLiteral
  , boolLiteral
  , reserved
  , operator
  , lvalue
  , number
  , identLetter
  ) where
  
import           Data.Functor (void)
import           Data.Void (Void)
import           Data.Int (Int32)
import           Data.Char (isAscii, isAsciiLower, isAsciiUpper, isDigit)
import           Numeric (showHex)

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space l2Space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "//"
    blockComment = L.skipBlockCommentNested "/*" "*/"

isL2Whitespace :: Char -> Bool
isL2Whitespace c = c == ' ' || c == '\t' || c == '\r' || c == '\n'

l2Space1 :: Parser ()
l2Space1 = void $ some (satisfy isL2Whitespace <?> "whitespace")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

semi :: Parser ()
semi = void $ symbol ";"

numberLiteral :: Parser String
numberLiteral = lexeme (try hexLiteral <|> decLiteral <?> "number")

boolLiteral :: Parser Bool
boolLiteral = do
  literal <- try (string "true") <|> string "false"
  return $ case literal of
    "true" -> True
    "false" -> False

-- We want to reject leading zeroes, but `0` itself should of course be accepted
decLiteral :: Parser String
decLiteral = string "0" <|> (:) <$> oneOf ['1'..'9'] <*> many digitChar

hexLiteral :: Parser String
hexLiteral = do
  void $ string' "0x"
  digits <- some hexDigitChar
  return ("0x" ++ digits)

number :: Parser Integer
number = try hexadecimal <|> decimal <?> "number"

decimal :: Parser Integer
decimal = do
  n <- lexeme L.decimal
  notFollowedBy alphaNumChar
  if n < maxInt
    then return n
    else if n == maxInt
           then return (-maxInt)
           else fail $ "Decimal literal out of bounds: " ++ show n
  where maxInt = 2^(31 :: Integer)

hexadecimal :: Parser Integer
hexadecimal = do
  void $ string' "0x"
  n <- lexeme L.hexadecimal
  if n > maxHex
    then fail $ "Hexadecimal literal out of bounds: " ++ "0x" ++ showHex n ""
    else return $ toInteger ((fromInteger n) :: Int32)
  where maxHex = 0xFFFFFFFF

reserved :: String -> Parser ()
reserved w = void $ lexeme $ (string w <* notFollowedBy identLetter)

reservedWords :: [String]
reservedWords =
  [ "alloc"
  , "alloc_array"
  , "assert"
  , "bool"
  , "break"
  , "char"
  , "continue"
  , "else"
  , "false"
  , "for"
  , "if"
  , "int"
  , "NULL"
  , "print"
  , "read"
  , "return"
  , "string"
  , "struct"
  , "true"
  , "void"
  , "while"
  ]

-- Operations
opStart :: Parser Char
opStart = oneOf "=+-*/%&^|<>!~"

opLetter :: Parser Char
opLetter = oneOf "=&|<>"

operator :: Parser String
operator = lexeme ((:) <$> opStart <*> many opLetter)

-- Identifiers
identStart :: Parser Char
identStart = satisfy isIdentStart
  where
    isIdentStart c = isAscii c && (isAsciiUpper c || isAsciiLower c || c == '_')

identLetter :: Parser Char
identLetter = satisfy isIdentLetter
  where
    isIdentLetter c = isAscii c && (isAsciiUpper c || isAsciiLower c || isDigit c || c == '_')

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> identStart <*> many identLetter
    check x =
      if x `elem` reservedWords
        then fail (x ++ " is reserved")
        else return x

lvalue :: Parser String
lvalue = try identifier <|> parens lvalue <?> "lvalue"
