module Compile.Semantic.ReturnAnalysis
  ( checkReturns
  ) where
  
import Compile.Semantic.Util
import Compile.Frontend.AST
import Compile.Semantic.Traverse
import Compile.Semantic.TraversalStates

import Control.Monad (unless)


checkReturns :: Handler NoState
checkReturns = defaultHandler
  { hFuncExit = functionReturns
  }

functionReturns :: Function -> NoState ()
functionReturns (Function _ _ _ block _) = unless (blockReturns block) $ semanticFail' ("Not all control-flow paths return a value")

blockReturns :: Block -> Bool
blockReturns (Block stmts _) = stmtsReturn stmts

stmtsReturn :: [Stmt] -> Bool
stmtsReturn []     = False
stmtsReturn (s:ss) = stmtReturns s || stmtsReturn ss

stmtReturns :: Stmt -> Bool
stmtReturns stmt = case stmt of
  Ret _ _ -> True
  
  If _ thenStmt mElseStmt _ ->
    case mElseStmt of
      Nothing -> False
      Just elseStmt -> stmtReturns thenStmt && stmtReturns elseStmt
  
  InnerBlock blk _ -> blockReturns blk
  
  _       -> False