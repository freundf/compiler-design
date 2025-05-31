module Compile.Semantic.VariableAnalysis
  ( varStatusAnalysis
  ) where
  
import           Compile.Frontend.AST (AST(..), Block(..), Expr(..), Stmt(..), Type(..), binOpType, unOpType, posPretty)
import           Compile.Frontend.Parser (parseNumber)
import           Compile.Semantic.Util
import           Error (L1ExceptT, semanticFail)

import           Control.Monad (unless, when, void)
import           Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as Map

import           Text.Megaparsec.Pos (SourcePos)


varStatusAnalysis :: AST -> Semantic ()
varStatusAnalysis (Program block) = checkBlock block

checkBlock :: Block -> Semantic ()
checkBlock (Block stmts _) = localScope $ mapM_ checkStmt stmts

checkStmt :: Stmt -> Semantic ()
checkStmt stmt = case stmt of
  Decl ty name pos -> do
    scope <- gets (head . scopes)
    when (Map.member name scope) $ semanticFail' ("Redeclaration of '" ++ name ++ "' at " ++ posPretty pos)
    let newScope = Map.insert name (VarInfo ty False) scope
    modify $ \s -> s { scopes = newScope : tail (scopes s) }
  
  Init ty name expr pos -> do
    scope <- gets (head . scopes)
    when (Map.member name scope) $ semanticFail' ("Redeclaration of '" ++ name ++ "' at " ++ posPretty pos)
    t <- checkExpr expr
    when (t /= ty) $ semanticFail' ("Type mismatch initializing '" ++ name ++ "' at " ++ posPretty pos)
    let newScope = Map.insert name (VarInfo ty True) scope
    modify $ \s -> s { scopes = newScope : tail (scopes s) }
    
  Asgn name op expr pos -> do
    var <- lookupVar name
    case var of
      Nothing -> semanticFail' ("Assignement to undeclared '" ++ name ++ "' at " ++  posPretty pos)
      Just (VarInfo ty initialized) -> do
        case op of
          Nothing -> do
            t <- checkExpr expr
            when (t /= ty) $ semanticFail' ("Type mismatch in assignment to '" ++ name ++ "' at " ++ posPretty pos)
            updateVar name (VarInfo ty True)
          Just bop -> do
            unless initialized $ semanticFail' ("Compound assignment before initialization of '" ++ name ++ "' at " ++ posPretty pos)
            let (_, tOut) = binOpType bop
            when (tOut /= ty) $ semanticFail' ("Invalid operator for variable '" ++ name ++ "' at " ++ posPretty pos)
            t <- checkExpr expr
            when (t /= ty) $ semanticFail' ("Type mismatch in compound assignment to '" ++ name ++ "' at " ++ posPretty pos)

  Ret expr pos -> void (checkExpr expr)
  
  If cond body elseBody pos -> do
    checkExprType cond TBool pos
    checkStmt body
    maybe (pure ()) checkStmt elseBody
  
  While cond body pos -> do
    checkExprType cond TBool pos
    inLoop (checkStmt body)
    
  For fInit cond fStep body pos -> do
    maybe (pure ()) checkStmt fInit
    checkExprType cond TBool pos
    case fStep of
      Just (Decl _ name pos) -> semanticFail' ("Cannot declare '" ++ name ++ "'in for-loop step at " ++ prettyPos pos)
      Nothing -> maybe (pure ()) checkStmt fStep
    inLoop (checkStmt body)
    
  Break pos -> checkInLoop pos
  Continue pos -> checkInLoop pos
  InnerBlock blk _ -> localScope (checkBlock blk)
  
  
checkExpr :: Expr -> Semantic Type
checkExpr expr = case expr of
  BoolLit _ pos -> return TBool
  
  IntExpr s pos -> case parseNumber s of
    Left e -> semanticFail' ("Bad int literal at " ++ posPretty pos ++ ": " ++ e)
    Right _ -> return TInt
    
  Ident name pos -> do
    var <- lookupVar name
    case var of
      Nothing -> semanticFail' ("Use of undeclared '" ++ name ++ "' at " ++ posPretty pos)
      Just (VarInfo ty initialized) -> do
        unless initialized $ semanticFail' ("Use of uninitialized '" ++ name ++ "' at " ++ posPretty pos)
        return ty
        
  UnExpr uop e -> do
    let (tIn, tOut) = unOpType uop
    t <- checkExpr e
    when (tIn /= t) $ semanticFail' ("Invalid unary operation " ++ show uop ++ " on " ++ show t)
    return tOut
    
  BinExpr bop e1 e2 -> do
    let (tIn, tOut) = binOpType bop
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    when (tIn /= (t1, t2)) $ semanticFail' ("Type error in operation " ++ show bop)
    return tOut
    
  Ternary cond e1 e2 -> do
    tCond <- checkExpr cond
    when (tCond /= TBool) $ semanticFail' ("Ternary condition not boolean")
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    if t1 == t2
      then return t1
      else semanticFail' ("Mismatching types in ternary branches")
  
  
checkExprType :: Expr -> Type -> SourcePos -> Semantic ()
checkExprType e ty pos = do
  t <- checkExpr e
  when (t /= ty) $ semanticFail' ("Expected " ++ show ty ++ " at " ++ posPretty pos)
  
  
checkInLoop :: SourcePos -> Semantic ()
checkInLoop pos = do
  depth <- gets loopDepth
  when (depth <= 0) $ semanticFail' ("'break' or 'continue' not within loop at " ++ posPretty pos)

localScope :: Semantic a -> Semantic a
localScope m = do
  modify $ \s -> s { scopes = Map.empty : scopes s}
  res <- m
  modify $ \s -> s { scopes = tail (scopes s) }
  return res
  
lookupVar :: String -> Semantic (Maybe VarInfo)
lookupVar name = gets (findInScopes name . scopes)
  where
    findInScopes _ [] = Nothing
    findInScopes x (m:ms) = case Map.lookup x m of
      Just v -> Just v
      Nothing -> findInScopes x ms

updateVar :: String -> VarInfo -> Semantic ()
updateVar name info = do
  scps <- gets scopes
  let update [] = []
      update (m:ms) = if Map.member name m
                    then Map.insert name info m : ms
                    else m : update ms
  modify $ \s -> s { scopes = update scps}

inLoop :: Semantic a -> Semantic a
inLoop m = do
  modify $ \s -> s { loopDepth = loopDepth s + 1}
  res <- m
  modify $ \s -> s { loopDepth = loopDepth s - 1}
  return res
