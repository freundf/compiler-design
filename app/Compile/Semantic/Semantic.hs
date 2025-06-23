module Compile.Semantic.Semantic
  ( semanticAnalysis
  ) where

import           Compile.Frontend.AST (AST(..), Block(..), Expr(..), Stmt(..), Type(..), binOpType, unOpType, posPretty)
import           Compile.Frontend.Parser (parseNumber)
import           Error (L1ExceptT, semanticFail)
import           Compile.Semantic.VariableAnalysis (varStatusAnalysis)
import           Compile.Semantic.ReturnAnalysis (checkReturns)
import           Compile.Semantic.TypeAnalysis (typeCheck)
import           Compile.Semantic.NameAnalysis (resolveNames)
import           Compile.Semantic.BreakContinueAnalysis (checkBreakContinue)
import           Compile.Semantic.ForAnalysis (analyseFor)
import           Compile.Semantic.IntegerAnalysis (checkIntegers)
import           Compile.Semantic.Traverse (traverseFunction, chainHandlers, TraversalOrder(..))
import           Compile.Semantic.Util

import           Control.Monad (unless, when, void)
import           Control.Monad.State.Strict
import           Data.Map (Map)
import qualified Data.Map as Map

import           Text.Megaparsec.Pos (SourcePos)



semanticAnalysis :: AST -> L1ExceptT ()
semanticAnalysis (f:fs) = do
  let initialCtx = Context { scopes = [], oldScopes = [], loopDepth = 0, returnType = TAny, recordedTypes = [] }
      checkVarStatus = chainHandlers [resolveNames, varStatusAnalysis, analyseFor]
      checkTypes = chainHandlers [resolveNames, typeCheck]
  runStateT (traverseFunction PostOrder checkVarStatus f) initialCtx
  runStateT (traverseFunction PostOrder checkTypes f) initialCtx
  runStateT (traverseFunction PostOrder checkIntegers f) initialCtx
  runStateT (traverseFunction PostOrder checkBreakContinue f) initialCtx
  runStateT (traverseFunction PostOrder checkReturns f) initialCtx
  return ()



