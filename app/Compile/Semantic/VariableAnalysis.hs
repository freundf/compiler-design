module Compile.Semantic.VariableAnalysis
  ( varStatusAnalysis
  ) where
  
import Compile.Frontend.AST
import Compile.Semantic.Util
import Compile.Semantic.Traverse

import Control.Monad (unless, when)
import Data.Maybe (isJust)

varStatusAnalysis :: Handler Sem
varStatusAnalysis = defaultHandler
  { hAsgn = varStatusAsgn
  , hIdent = varStatusIdent
  , hRet = varStatusRet
  , hBreak = varStatusBreak
  , hContinue = varStatusContinue
  }

varStatusAsgn :: String -> AsgnOp -> Expr -> SourcePos -> Semantic ()
varStatusAsgn name op _ pos = do
  VarInfo ty initialized <- lookupVar name pos
  when (not initialized && isJust op) $
    semanticFail' $ "Assign: Use of uninitialized variable '" ++ name ++ "' at " ++ posPretty pos
  updateVar name (VarInfo ty True)
  
varStatusIdent :: String -> SourcePos -> Semantic ()
varStatusIdent name pos = do
  VarInfo _ initialized <- lookupVar name pos
  unless initialized $
    semanticFail' $ "Identifier: Use of uninitialized variable '" ++ name ++ "' at " ++ posPretty pos

varStatusRet :: Expr -> SourcePos -> Semantic ()
varStatusRet _ _ = initializeAll

varStatusBreak :: SourcePos -> Semantic ()
varStatusBreak _ = initializeAll

varStatusContinue :: SourcePos -> Semantic ()
varStatusContinue _ = initializeAll
