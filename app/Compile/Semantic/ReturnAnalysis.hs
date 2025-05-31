module Compile.Semantic.ReturnAnalysis
  ( analyzeReturns
  ) where
  
import           Compile.Semantic.Util


checkReturns :: AST -> Semantic ()
checkReturns (Program block) = unless (returnsBlock stmts) $ semanticFail' ("Not all control-flow paths return a value")


returnsBlock :: Block -> Bool
returnsBlock (Block stmts _) = any returnsStmt stmts

returnsStmt :: Stmt -> Bool
returnsStmt stmt = case stmt of
  Ret _ _                     -> True
  If _ t e _                  -> returnsStmt t && maybe False returnsStmt e
  InnerBlock (Block ss _ ) _  -> returnsBlock ss
  _                           -> False