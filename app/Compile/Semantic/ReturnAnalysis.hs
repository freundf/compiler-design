module Compile.Semantic.ReturnAnalysis
  ( checkReturns
  ) where
  
import Compile.Semantic.Util
import Compile.Frontend.AST
import Compile.Semantic.Traverse

import Control.Monad (unless)


checkReturns :: Handler Sem
checkReturns = defaultHandler
  { hFuncExit = functionReturns
  }

functionReturns :: AST -> Semantic ()
functionReturns (Function block) = unless (blockReturns block) $ semanticFail' ("Not all control-flow paths return a value")

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