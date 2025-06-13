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
import           Compile.Semantic.Traverse (traverseAST, chainHandlers, TraversalOrder(..))
import           Compile.Semantic.Util

import           Control.Monad (unless, when, void)
import           Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as Map

import           Text.Megaparsec.Pos (SourcePos)



semanticAnalysis :: AST -> L1ExceptT ()
semanticAnalysis ast = do
  let initialCtx = Context { scopes = [], loopDepth = 0, returnType = TAny, recordedTypes = [] }
      firstRun = chainHandlers [resolveNames, varStatusAnalysis]
      secondRun = chainHandlers
        [ resolveNames
        , checkIntegers
        , typeCheck
        , checkBreakContinue
        , analyseFor
        , checkReturns
        ]
  runStateT (traverseAST PreOrder firstRun ast) initialCtx
  runStateT (traverseAST PostOrder secondRun ast) initialCtx
  return ()



