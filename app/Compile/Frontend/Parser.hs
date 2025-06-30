module Compile.Frontend.Parser
  ( parseAST,
    parseNumber
  ) where

import           Compile.Frontend.AST (AST, Function(..), Block(..), Stmt(..), Expr(..), BinOp(..), UnOp(..), Type(..))
import           Compile.Frontend.Lexer
import           Error (L1ExceptT, parserFail)

import           Control.Monad.Combinators.Expr
import           Control.Monad.IO.Class (liftIO)
import           Data.Functor (void)
import           Data.Void (Void)
import           Data.Maybe (fromMaybe)

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


parseAST :: FilePath -> L1ExceptT AST
parseAST path = do
  text <- liftIO $ readFile path
  case parse astParser path text of
    Left err -> parserFail $ errorBundlePretty err
    Right ast -> return ast

parseNumber :: String -> Either String Integer
parseNumber s = do
  case parse number "<literal>" s of
    Left err -> Left $ errorBundlePretty err
    Right n -> Right n

type Parser = Parsec Void String

astParser :: Parser AST
astParser = do
  sc
  ast <- many function
  eof
  return $ ast

function :: Parser Function
function = do
  pos <- getSourcePos
  retTy <- ty
  sc
  name <- identifier
  params <- paramList
  body <- block
  return $ Function retTy name params body pos

paramList :: Parser [(Type, String)]
paramList = do
  parens $ do
    (pure [] <* notFollowedBy param)
    <|> ((:) <$> param <*> paramListFollow)

paramListFollow :: Parser [(Type, String)]
paramListFollow = do
  many (symbol "," *> param)

param :: Parser (Type, String)
param = do
  t <- ty
  sc
  name <- identifier
  return (t, name)

argList :: Parser [Expr]
argList = do
  parens $ do
    (pure [] <* notFollowedBy arg)
    <|> ((:) <$> arg <*> argListFollow)

argListFollow :: Parser [Expr]
argListFollow = many (symbol "," *> arg)

arg :: Parser Expr
arg = expr

block :: Parser Block
block = do
  pos <- getSourcePos
  stmts <- braces $ many stmt
  return $ Block stmts pos

innerBlock :: Parser Stmt
innerBlock = do
  pos <- getSourcePos
  blk <- block
  return $ InnerBlock blk pos

stmt :: Parser Stmt
stmt = try innerBlock <|> try control <|> (simp <* semi)

control :: Parser Stmt
control = try cIf <|> try cWhile <|> try cFor <|> try cContinue <|> try cBreak <|> ret

cWhile :: Parser Stmt
cWhile = do
  pos <- getSourcePos
  reserved "while"
  cond <- parens expr
  body <- stmt
  return $ While cond body pos

cFor :: Parser Stmt
cFor = do
  pos <- getSourcePos
  reserved "for"
  (fInit, cond, step) <- parens $ do
    fInit <- optional simp
    semi
    cond <- expr
    semi
    step <- optional simp
    return (fInit, cond, step)
  body <- stmt
  return $ For fInit cond step body pos

cIf :: Parser Stmt
cIf = do
  pos <- getSourcePos
  reserved "if"
  cond <- parens expr
  body <- stmt
  elseBody <- optional cElse
  return $ If cond body elseBody pos

cElse :: Parser Stmt
cElse = do
  reserved "else"
  stmt

cContinue :: Parser Stmt
cContinue = do
  pos <- getSourcePos
  reserved "continue"
  semi
  return $ Continue pos

cBreak :: Parser Stmt
cBreak = do
  pos <- getSourcePos
  reserved "break"
  semi
  return $ Break pos

decl :: Parser Stmt
decl = try declInit <|> declNoInit

declNoInit :: Parser Stmt
declNoInit = do
  pos <- getSourcePos
  t <- ty
  sc
  name <- identifier
  
  return $ Decl t name pos

declInit :: Parser Stmt
declInit = do
  pos <- getSourcePos
  t <- ty
  sc
  name <- identifier
  void $ symbol "="
  e <- expr
  return $ Init t name e pos

ty :: Parser Type
ty =  (pure TInt <* (string "int" <* notFollowedBy identLetter))
  <|> (pure TBool <* (string "bool" <* notFollowedBy identLetter))

simp :: Parser Stmt
simp = do
  s <- try asgn <|> try decl <|> call
  return s

call :: Parser Stmt
call = do
  pos <- getSourcePos
  name <- identifier
  sc
  args <- argList
  return $ Call name args pos

callExpr :: Parser Expr
callExpr = do
  pos <- getSourcePos
  name <- identifier
  sc
  args <- argList
  return $ CallExpr name args pos

asgn :: Parser Stmt
asgn = do
  pos <- getSourcePos
  name <- lvalue
  op <- asnOp
  e <- expr
  return $ Asgn name op e pos

asnOp :: Parser (Maybe BinOp)
asnOp = do
  op <- operator
  case op of
    "+=" -> pure (Just Add)
    "*=" -> pure (Just Mul)
    "-=" -> pure (Just Sub)
    "/=" -> pure (Just Div)
    "%=" -> pure (Just Mod)
    "&=" -> pure (Just BitAnd)
    "|=" -> pure (Just BitOr)
    "^=" -> pure (Just BitXor)
    "<<=" -> pure (Just Shl)
    ">>=" -> pure (Just Shr)
    "=" -> pure Nothing
    x -> fail $ "Nonexistent assignment operator: " ++ x
  <?> "assignment operator"

ret :: Parser Stmt
ret = do
  pos <- getSourcePos
  reserved "return"
  e <- expr
  semi
  return $ Ret e pos

expr' :: Parser Expr
expr' = parens expr <|> boolLit <|> intExpr <|> try callExpr <|> identExpr

boolLit :: Parser Expr
boolLit = do
  pos <- getSourcePos
  b <- boolLiteral
  return $ BoolLit b pos

intExpr :: Parser Expr
intExpr = do
  pos <- getSourcePos
  str <- numberLiteral
  return $ IntExpr str pos

identExpr :: Parser Expr
identExpr = do
  pos <- getSourcePos
  name <- identifier
  return $ Ident name pos

opTable :: [[Operator Parser Expr]]
opTable =
  [ [Prefix manyUnaryOp]
  , [ InfixL (BinExpr Mul <$ symbol "*")
    , InfixL (BinExpr Div <$ symbol "/")
    , InfixL (BinExpr Mod <$ symbol "%")
    ]
  , [ InfixL (BinExpr Add <$ symbol "+")
    , InfixL (BinExpr Sub <$ symbol "-")
    ]
  , [ InfixL (BinExpr Shl <$ symbol "<<")
    , InfixL (BinExpr Shr <$ symbol ">>")
    ]
  , [ InfixL (BinExpr Leq <$ symbol "<=")
    , InfixL (BinExpr Lt  <$ symbol "<")
    , InfixL (BinExpr Geq <$ symbol ">=")
    , InfixL (BinExpr Gt  <$ symbol ">")
    ]
  , [ InfixL (BinExpr Eq  <$ symbol "==")
    , InfixL (BinExpr Neq <$ symbol "!=")
    ]
  , [ InfixL (BinExpr BitAnd <$ try (symbol "&" <* notFollowedBy (char '&'))) ]
  , [ InfixL (BinExpr BitXor <$ symbol "^") ]
  , [ InfixL (BinExpr BitOr  <$ try (symbol "|" <* notFollowedBy (char '|'))) ]
  , [ InfixL (BinExpr And    <$ symbol "&&") ]
  , [ InfixL (BinExpr Or     <$ symbol "||") ]
  , [ TernR ((Ternary <$ symbol ":") <$ symbol "?") ]
  ]
  where
    -- this allows us to parse `---x` as `-(-(-x))`
    -- makeExprParser doesn't do this by default
    manyUnaryOp = foldr1 (.) <$> some unaryOp

unaryOp :: Parser (Expr -> Expr)
unaryOp = (UnExpr Neg <$ symbol "-")
        <|> (UnExpr Not <$ symbol "!")
        <|> (UnExpr BitNot <$ symbol "~")

expr :: Parser Expr
expr = makeExprParser expr' opTable <?> "expression"