module Compile.Semantic.VariableAnalysis
  ( varStatusAnalysis
  ) where
  
import Compile.Frontend.AST
import Compile.Semantic.Util
import Compile.Semantic.Traverse
import Compile.Semantic.TraversalStates

import Control.Monad (unless, when)
import Control.Monad.State.Strict
import qualified Data.Set as Set
import Data.Maybe (isJust)

import Debug.Trace (traceM)

varStatusAnalysis :: Handler VariableState
varStatusAnalysis = defaultHandler
  { hAsgn = varStatusAsgn
  , hDecl = varStatusDecl
  , hInit = varStatusInit
  , hIdent = varStatusIdent
  , hIf = varStatusIf
  , hIfThen = varStatusIfThen
  , hIfElse = varStatusIfElse
  , hRet = varStatusRet
  , hBreak = varStatusBreak
  , hContinue = varStatusContinue
  }

varStatusAsgn :: String -> AsgnOp -> Expr -> SourcePos -> VariableState ()
varStatusAsgn name op _ pos = do
  when (isJust op) $ check name pos
  define name pos

varStatusDecl :: Type -> String -> SourcePos -> VariableState ()
varStatusDecl _ name pos = declare name pos

varStatusInit :: Type -> String -> Expr -> SourcePos -> VariableState ()
varStatusInit _ name _ pos = do
  declare name pos
  define name pos

varStatusIdent :: String -> SourcePos -> VariableState ()
varStatusIdent name pos = check name pos

varStatusIfThen :: Stmt -> VariableState ()
varStatusIfThen _ = do
  scope <- getCurrentScope
  s <- get
  traceM (show s)
  modify $ \s -> s { scopeIf = scope : (scopeIf s) }

varStatusIfElse :: Maybe Stmt -> VariableState ()
varStatusIfElse mElse = do
  scope <- getCurrentScope
  let newScope = case mElse of
                  Just _ -> scope
                  Nothing -> Scope Set.empty Set.empty
  modify $ \s -> s { scopeElse = newScope : (scopeElse s) }

varStatusIf :: Expr -> Stmt -> Maybe Stmt -> SourcePos -> VariableState ()
varStatusIf _ _ _ _ = do
  ifScope <- gets (head . scopeIf)
  elseScope <- gets (head . scopeElse)
  modify $ \s -> s { scopeIf = tail (scopeIf s), scopeElse = tail (scopeElse s) }
  scope <- gets (head . scopes)
  traceM (show ifScope)
  traceM (show elseScope)
  traceM (show scope)
  let updated = scope { definitions = (definitions scope) `Set.union` ((definitions ifScope) `Set.intersection` (definitions elseScope)) }
  modify $ \s -> s { scopes = updated : tail (scopes s) }

varStatusRet :: Expr -> SourcePos -> VariableState ()
varStatusRet _ _ = initializeAll

varStatusBreak :: SourcePos -> VariableState ()
varStatusBreak _ = initializeAll

varStatusContinue :: SourcePos -> VariableState ()
varStatusContinue _ = initializeAll
