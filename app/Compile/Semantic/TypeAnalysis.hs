module Compile.Semantic.TypeAnalysis
  ( typeCheck
  ) where
  
import Compile.Frontend.AST
import Compile.Semantic.Util
import Compile.Semantic.Traverse

import Control.Monad (unless)
import Control.Monad.State


typeCheck :: Handler Sem
typeCheck = defaultHandler
  { hFuncEnter = recordFunctionReturnType
  , hInit = checkInit
  , hAsgn = checkAsgn
  , hRet = checkRet
  , hWhile = checkWhile
  , hFor = checkFor
  , hIf = checkIf
  , hBoolLit = checkBoolLit
  , hIntExpr = checkIntExpr
  , hIdent = checkIdent
  , hUnExpr = checkUnExpr
  , hBinExpr = checkBinExpr
  , hTernary = checkTernary
  }

recordFunctionReturnType :: AST -> Semantic ()
recordFunctionReturnType _ = modify $ \s -> s { returnType = TInt }

checkInit :: Type -> String -> Expr -> SourcePos -> Semantic ()
checkInit ty _ _ pos = do
  [t] <- popTypes 1
  unless (ty == t) $
    semanticFail' $ "Initialization type mismatch at " ++ posPretty pos ++ ": declared " ++ show ty ++ ", got " ++ show t
  
checkAsgn :: String -> AsgnOp -> Expr -> SourcePos -> Semantic ()
checkAsgn name _ _ pos = do
  VarInfo ty _ <- lookupVar name pos
  [t] <- popTypes 1
  unless (ty == t) $
    semanticFail' $ "Assignment type mismatch to '" ++ name ++ "' at " ++ posPretty pos ++ ": declared " ++ show ty ++ ", got " ++ show t

checkRet :: Expr -> SourcePos -> Semantic ()
checkRet _ pos = do
  ty <- gets returnType
  [t] <- popTypes 1
  unless (ty == t) $
    semanticFail' $ "Return type mismatch at " ++ posPretty pos ++ ": expected " ++ show ty ++ ", got " ++ show t

checkWhile :: Expr -> Stmt -> SourcePos -> Semantic ()
checkWhile _ _ pos = do
  [t] <- popTypes 1
  unless (t == TBool) $
    semanticFail' $ "While condition must be boolean at " ++ posPretty pos
  
checkFor :: Maybe Stmt -> Expr -> Maybe Stmt -> Stmt -> SourcePos -> Semantic ()
checkFor _ _ _ _ pos = do
  [t] <- popTypes 1
  unless (t == TBool) $
    semanticFail' $ "For condition must be boolean at " ++ posPretty pos

checkIf :: Expr -> Stmt -> Maybe Stmt -> SourcePos -> Semantic ()
checkIf _ _ _ pos = do
  [t] <- popTypes 1
  unless (t == TBool) $
    semanticFail' $ "If condition must be boolean at " ++ posPretty pos

checkBoolLit :: Bool -> SourcePos -> Semantic ()
checkBoolLit _ pos = pushType TBool

checkIntExpr :: String -> SourcePos -> Semantic ()
checkIntExpr _ _ = pushType TInt

checkIdent :: String -> SourcePos -> Semantic ()
checkIdent name pos = do
  VarInfo ty _ <- lookupVar name pos
  pushType ty

checkUnExpr :: UnOp -> Expr -> Semantic ()
checkUnExpr op _ = do
  let (tIn, tOut) = unOpType op
  [t] <- popTypes 1
  unless (t == tIn) $
    semanticFail' $ "Unary " ++ show op ++ ": expected " ++ show tIn ++ ", got " ++ show t
  pushType tOut
  
checkBinExpr :: BinOp -> Expr -> Expr -> Semantic ()
checkBinExpr op _ _ = do
  let (tIn, tOut) = binOpType op
  [t2, t1] <- popTypes 2
  unless ((t1, t2) == tIn) $
    semanticFail' $ "Binary " ++ show op ++ ": expected " ++ show tIn ++ ", got (" ++ show t1 ++ "," ++ show t2 ++ ")"
  pushType tOut
  
checkTernary :: Expr -> Expr -> Expr -> Semantic ()
checkTernary _ _ _ = do
  [t2, t1, c] <- popTypes 3
  unless (c == TBool) $
    semanticFail' $ "Ternary condition must be boolean"
  unless (t1 == t2) $
    semanticFail' $ "Ternary types must match: " ++ show t1 ++ " vs " ++ show t2
  pushType t1