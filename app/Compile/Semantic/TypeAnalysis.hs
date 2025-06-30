module Compile.Semantic.TypeAnalysis
  ( checkTypes
  ) where
  
import Compile.Frontend.AST
import Compile.Semantic.Util
import Compile.Semantic.Traverse
import Compile.Semantic.TraversalStates

import Control.Monad (unless)
import Control.Monad.State.Strict
import Data.List (find)


checkTypes :: Handler TypeState
checkTypes = defaultHandler
  { hAST = recordFunctions
  , hFuncEnter = recordFunctionReturnType
  , hDecl = checkDecl
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
  , hCall = checkCall
  , hCallExpr = checkCallExpr
  }

recordFunctions :: AST -> TypeState ()
recordFunctions ast = modify $ \s -> s { functions = ast }

recordFunctionReturnType :: Function -> TypeState ()
recordFunctionReturnType f = registerReturnType (retType f)

checkDecl :: Type -> String -> SourcePos -> TypeState ()
checkDecl ty name _ = registerType name ty

checkInit :: Type -> String -> Expr -> SourcePos -> TypeState ()
checkInit ty name _ pos = do
  registerType name ty
  [t] <- popTypes 1
  unless (ty == t) $
    semanticFail' $ "Initialization type mismatch at " ++ posPretty pos ++ ": declared " ++ show ty ++ ", got " ++ show t
  
checkAsgn :: String -> AsgnOp -> Expr -> SourcePos -> TypeState ()
checkAsgn name op _ pos = do
  ty <- getType name pos
  [t] <- popTypes 1
  case op of
    Just bop -> do
      let (tIn, tOut) = binOpType bop
      unless ((ty, t) `elem` tIn) $
        semanticFail' $ "Type mismatch for assignment operator '" ++ show bop ++ "' at " ++ posPretty pos ++ ": expected " ++ show tIn ++ ", got (" ++ show ty ++ ", " ++ show t ++ ")"
      unless (ty == tOut) $
        semanticFail' $ "Assignment type mismatch to '" ++ name ++ "' at " ++ posPretty pos ++ ": declared " ++ show ty ++ ", got " ++ show tOut
    Nothing -> unless (ty == t) $
      semanticFail' $ "Assignment type mismatch to '" ++ name ++ "' at " ++ posPretty pos ++ ": declared " ++ show ty ++ ", got " ++ show t

checkRet :: Expr -> SourcePos -> TypeState ()
checkRet _ pos = do
  ty <- gets returnType
  [t] <- popTypes 1
  unless (ty == t) $
    semanticFail' $ "Return type mismatch at " ++ posPretty pos ++ ": expected " ++ show ty ++ ", got " ++ show t

checkWhile :: Expr -> Stmt -> SourcePos -> TypeState ()
checkWhile _ _ pos = do
  [t] <- popTypes 1
  unless (t == TBool) $
    semanticFail' $ "While condition must be boolean at " ++ posPretty pos
  
checkFor :: Maybe Stmt -> Expr -> Maybe Stmt -> Stmt -> SourcePos -> TypeState ()
checkFor _ _ _ _ pos = do
  [t] <- popTypes 1
  unless (t == TBool) $
    semanticFail' $ "For condition must be boolean at " ++ posPretty pos

checkIf :: Expr -> Stmt -> Maybe Stmt -> SourcePos -> TypeState ()
checkIf _ _ _ pos = do
  [t] <- popTypes 1
  unless (t == TBool) $
    semanticFail' $ "If condition must be boolean at " ++ posPretty pos

checkBoolLit :: Bool -> SourcePos -> TypeState ()
checkBoolLit _ pos = pushType TBool

checkIntExpr :: String -> SourcePos -> TypeState ()
checkIntExpr _ _ = pushType TInt

checkIdent :: String -> SourcePos -> TypeState ()
checkIdent name pos = do
  ty <- getType name pos
  pushType ty

checkUnExpr :: UnOp -> Expr -> TypeState ()
checkUnExpr op _ = do
  let (tIn, tOut) = unOpType op
  [t] <- popTypes 1
  unless (t `elem` tIn) $
    semanticFail' $ "Unary " ++ show op ++ ": expected " ++ show tIn ++ ", got " ++ show t
  pushType tOut
  
checkBinExpr :: BinOp -> Expr -> Expr -> TypeState ()
checkBinExpr op _ _ = do
  let (tIn, tOut) = binOpType op
  [t2, t1] <- popTypes 2
  unless ((t1, t2) `elem` tIn) $
    semanticFail' $ "Binary " ++ show op ++ ": expected " ++ show tIn ++ ", got (" ++ show t1 ++ "," ++ show t2 ++ ")"
  pushType tOut
  
checkTernary :: Expr -> Expr -> Expr -> TypeState ()
checkTernary _ _ _ = do
  [t2, t1, c] <- popTypes 3
  unless (c == TBool) $
    semanticFail' $ "Ternary condition must be boolean"
  unless (t1 == t2) $
    semanticFail' $ "Ternary types must match: " ++ show t1 ++ " vs " ++ show t2
  pushType t1


checkCall :: String -> [Expr] -> SourcePos -> TypeState ()
checkCall f p pos = do
  funcs <- gets functions
  let function = find ((f ==) . fName) funcs
  args <- popTypes (length p)
  case function of
    Nothing -> semanticFail' $ "Call to undefined function: " ++ f ++ ", at " ++ show pos
    Just func -> do
      unless ((map fst (params func)) == (reverse args)) $ semanticFail' $ "Type mismatch in function arguments for '" ++ show f ++ "', at" ++ show pos

checkCallExpr :: String -> [Expr] -> SourcePos -> TypeState ()
checkCallExpr f p pos = do
  funcs <- gets functions
  let function = find ((f ==) . fName) funcs
  args <- popTypes (length p)
  case function of
    Nothing -> semanticFail' $ "Call to undefined function: " ++ f ++ ", at " ++ show pos
    Just func -> do
      unless ((map fst (params func)) == (reverse args)) $ semanticFail' $ "Type mismatch in function arguments for '" ++ show f ++ "', at" ++ show pos
      pushType (retType func)
