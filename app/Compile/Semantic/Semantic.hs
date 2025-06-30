module Compile.Semantic.Semantic
  ( semanticAnalysis
  ) where

import           Compile.Frontend.AST (AST(..), Block(..), Expr(..), Stmt(..), Type(..), binOpType, unOpType, posPretty)
import           Compile.Frontend.Parser (parseNumber)
import           Error (L1ExceptT, semanticFail)
import           Compile.Semantic.VariableAnalysis (varStatusAnalysis)
import           Compile.Semantic.ReturnAnalysis (checkReturns)
import           Compile.Semantic.TypeAnalysis (checkTypes)
import           Compile.Semantic.BreakContinueAnalysis (checkBreakContinue)
import           Compile.Semantic.ForAnalysis (analyseFor)
import           Compile.Semantic.IntegerAnalysis (checkIntegers)
import           Compile.Semantic.FunctionAnalysis (checkFunctions)
import           Compile.Semantic.Traverse (traverseAST, chainHandlers, TraversalOrder(..))
import           Compile.Semantic.TraversalStates
import           Compile.Semantic.Util

import           Control.Monad (unless, when, void)
import           Control.Monad.State.Strict
import           Data.Map (Map)
import qualified Data.Map as Map

import           Text.Megaparsec.Pos (SourcePos)



semanticAnalysis :: AST -> L1ExceptT ()
semanticAnalysis ast = do
  let checkVarStatus = chainHandlers [varStatusAnalysis, analyseFor]
  runStateT (traverseAST PostOrder checkBreakContinue ast) emptyLoopState
  runStateT (traverseAST PostOrder checkFunctions ast) emptyFunctionState
  runStateT (traverseAST PostOrder checkIntegers ast) emptyNoState
  runStateT (traverseAST PostOrder checkReturns ast) emptyNoState
  runStateT (traverseAST PostOrder checkVarStatus ast) emptyVariableState
  runStateT (traverseAST PostOrder checkTypes ast) emptyTypeState
  return ()



