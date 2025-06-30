{-# LANGUAGE FlexibleInstances #-}
module Compile.Semantic.BreakContinueAnalysis
  ( checkBreakContinue
  ) where

import Compile.Semantic.Traverse
import Compile.Semantic.TraversalStates
import Compile.Semantic.Util
import Compile.Frontend.AST
import Error

import Control.Monad (when)
import Control.Monad.State.Strict



checkBreakContinue :: Handler LoopState
checkBreakContinue = defaultHandler
  { hBreak = checkBreak
  , hContinue = checkContinue
  }

checkBreak :: SourcePos -> LoopState ()
checkBreak pos = do
  depth <- get
  when (depth <= 0) $ semanticFail' ("'break' outside loop at " ++ posPretty pos)

checkContinue :: SourcePos -> LoopState ()
checkContinue pos = do
  depth <- get
  when (depth <= 0) $ semanticFail' ("'break' outside loop at " ++ posPretty pos)
