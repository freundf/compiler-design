module Compile.Semantic.ForAnalysis
  ( analyseFor
  ) where
  
import Compile.Semantic.Traverse
import Compile.Semantic.Util
import Compile.Frontend.AST

analyseFor :: Handler Sem
analyseFor = defaultHandler
  { hFor = checkForStep
  }
  
checkForStep :: Maybe Stmt -> Expr -> Maybe Stmt -> Stmt -> SourcePos -> Semantic ()
checkForStep _ _ mStep _ _ = case mStep of
  Just stepStmt -> case stepStmt of
     Decl _ name pos -> semanticFail' ("Can't declare '" ++ name ++ "' in for-loop step at " ++ posPretty pos)
     _ -> pure ()
  Nothing -> pure ()