module Compile.Semantic.BreakContinueAnalysis
  ( checkBreakContinue
  ) where

import Compile.Semantic.Traverse
import Compile.Semantic.Util
import Compile.Frontend.AST

import Control.Monad (when)
import Control.Monad.State.Strict

checkBreakContinue :: Handler Sem
checkBreakContinue = defaultHandler
  { hBreak = checkBreak
  , hContinue = checkContinue
  }
  
  
checkBreak :: SourcePos -> Semantic ()
checkBreak pos = do
  ctx <- get
  when (loopDepth ctx <= 0) $ semanticFail' ("'break' outside loop at " ++ posPretty pos)

checkContinue :: SourcePos -> Semantic ()
checkContinue pos = do
  ctx <- get
  when (loopDepth ctx <= 0) $ semanticFail' ("'break' outside loop at " ++ posPretty pos)
