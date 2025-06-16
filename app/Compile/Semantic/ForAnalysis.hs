module Compile.Semantic.ForAnalysis
  ( analyseFor
  ) where
  
import Compile.Semantic.Traverse
import Compile.Semantic.Util
import Compile.Frontend.AST

import Control.Monad (unless)
import Control.Monad.State.Strict
import Debug.Trace (traceM)

analyseFor :: Handler Sem
analyseFor = defaultHandler
  { hFor = checkForStep
  }
  
checkForStep :: Maybe Stmt -> Expr -> Maybe Stmt -> Stmt -> SourcePos -> Semantic ()
checkForStep _ _ mStep _ _ = case mStep of
  Just stepStmt -> case stepStmt of
    Decl _ name pos -> semanticFail' ("Can't declare '" ++ name ++ "' in for-loop step at " ++ posPretty pos)
    Init _ name _ pos -> semanticFail' ("Can't initialize '" ++ name ++ "' in for-loop step at " ++ posPretty pos)
    Asgn name _ _ pos -> do
      VarInfo _ initialized <- lookupVar name pos
      unless (initialized) $ semanticFail' ("Can't initialize '" ++ name ++ "' in for-loop step at " ++ posPretty pos)
    _ -> pure ()
  Nothing -> pure ()