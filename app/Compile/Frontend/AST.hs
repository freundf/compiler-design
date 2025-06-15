module Compile.Frontend.AST
  ( AST(..)
  , Block(..)
  , Stmt(..)
  , Expr(..)
  , AsgnOp(..)
  , BinOp(..)
  , UnOp(..)
  , Type(..)
  , showAsgnOp
  , unOpType
  , binOpType
  , posPretty
  , SourcePos
  ) where

import Data.List (intercalate)
import Text.Megaparsec

newtype AST = Function Block
  deriving (Eq, Show)

data Block = Block [Stmt] SourcePos
  deriving (Eq)

data Stmt
  -- Simple Statement
  = Decl Type String SourcePos
  | Init Type String Expr SourcePos
  | Asgn String AsgnOp Expr SourcePos
  | Ret Expr SourcePos
  -- Control Statement
  | While Expr Stmt SourcePos
  | For (Maybe Stmt) Expr (Maybe Stmt) Stmt SourcePos
  | If Expr Stmt (Maybe Stmt) SourcePos
  | Break SourcePos
  | Continue SourcePos
  -- Block Statement
  | InnerBlock Block SourcePos
  deriving (Eq)
  
data Type = TInt | TBool | TAny

data Expr
  = BoolLit Bool SourcePos
  | IntExpr String SourcePos
  | Ident String SourcePos
  | UnExpr UnOp Expr
  | BinExpr BinOp Expr Expr
  | Ternary Expr Expr Expr
  deriving (Eq)
  
-- Nothing means =, Just is for +=, %=, ...
type AsgnOp = Maybe BinOp

data BinOp
  = Mul | Add | Sub | Div | Mod
  | Lt | Leq | Gt | Geq
  | Eq | Neq
  | And | Or
  | BitAnd | BitOr | BitXor
  | Shl | Shr
  deriving (Eq)
  
data UnOp
  = Neg | Not | BitNot
  deriving (Eq)

-- re-exported for convenience
posPretty :: SourcePos -> String
posPretty = sourcePosPretty

-- Some very basic pretty printing
instance Show Block where
  show (Block stmts _) =
    "Block: {\n" ++ intercalate "\n" (map show stmts) ++ "\n}"

instance Show Stmt where
  show (Decl ty name _) = "Decl: " ++ show ty ++ " " ++ name
  show (Init ty name e _) = "Init: " ++ show ty ++ " " ++ name ++ " = " ++ show e
  show (Asgn name op e _) = "Assign: " ++ name ++ showAsgnOp op ++ show e
  show (Ret e _) = "Return: " ++ show e
  show (If cond thn els _) = "If (" ++ show cond ++ ") " ++ show thn ++ maybe "" ((" Else " ++) . show) els
  show (While cond body _) = "While (" ++ show cond ++ ") " ++ show body
  show (For mInit cond mStep body _) = "For (" ++ showOpt mInit ++ "; " ++ show cond ++ "; " ++ showOpt mStep ++ ") " ++ show body
    where
      showOpt (Just s) = show s
      showOpt Nothing = ""
  show (Break _) = "Break"
  show (Continue _) = "Continue"
  show (InnerBlock blk _) = show blk

instance Show Expr where
  show (BoolLit True _) = "true"
  show (BoolLit False _) = "false"
  show (IntExpr i _) = i
  show (Ident name _) = name
  show (UnExpr op e) = "(" ++ show op ++ " " ++ show e ++ ")"
  show (BinExpr op lhs rhs) = "(" ++ show lhs ++ " " ++ show op ++ " " ++ show rhs ++ ")"
  show (Ternary cond t f) = "(" ++ show cond ++ " ? " ++ show t ++ " : " ++ show f ++ ")"

instance Show BinOp where
  show Mul    = "*"
  show Add    = "+"
  show Sub    = "-"
  show Div    = "/"
  show Mod    = "%"
  show Lt     = "<"
  show Leq    = "<="
  show Gt     = ">"
  show Geq    = ">="
  show Eq     = "=="
  show Neq    = "!="
  show And    = "&&"
  show Or     = "||"
  show BitAnd = "&"
  show BitOr  = "|"
  show BitXor = "^"
  show Shl    = "<<"
  show Shr    = ">>"
  
instance Show UnOp where
  show Neg    = "-"
  show Not    = "!"
  show BitNot = "~"
  
instance Show Type where
  show TInt = "int"
  show TBool = "bool"
  
showAsgnOp :: AsgnOp -> String
showAsgnOp (Just op) = " " ++ show op ++ "= "
showAsgnOp _ = " = "

binOpType :: BinOp -> ((Type, Type), Type) -- (In, Out)
binOpType op
  | op `elem` [Mul, Add, Sub, Div, Mod, BitAnd, BitOr, BitXor, Shl, Shr]  = ((TInt, TInt), TInt)
  | op `elem` [Lt, Leq, Gt, Geq]                                          = ((TInt, TInt), TBool)
  | op `elem` [Eq, Neq]                                                   = ((TAny, TAny), TBool)
  | op `elem` [And, Or]                                                   = ((TBool, TBool), TBool)
  | otherwise                                                             = error $ "unknown binary operation: " ++ show op

unOpType :: UnOp -> (Type, Type) -- (In, Out)
unOpType op
  | op `elem` [Neg, BitNot] = (TInt, TInt)
  | op `elem` [Not]         = (TBool, TBool)
  | otherwise               = error $ "unknown unary operation: " ++ show op
  
-- TODO fix TAny for Eq, Neq
instance Eq Type where
  TAny  == _      = True
  _     == TAny   = True
  TInt  == TInt   = True
  TBool == TBool  = True
  _     == _      = False
  
  