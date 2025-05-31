module Compile.Semantic.Semantic
  ( semanticAnalysis
  ) where

import           Compile.Frontend.AST (AST(..), Block(..), Expr(..), Stmt(..), Type(..), binOpType, unOpType, posPretty)
import           Compile.Frontend.Parser (parseNumber)
import           Error (L1ExceptT, semanticFail)
import           Compile.Semantic.VariableAnalysis (varStatusAnalysis)
import           Compile.Semantic.ReturnAnalysis (checkReturns)
import           Compile.Semantic.Util

import           Control.Monad (unless, when, void)
import           Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as Map

import           Text.Megaparsec.Pos (SourcePos)


data VarInfo = VarInfo
  { vType :: Type
  , vInit :: Bool
  }
  
data Context = Context
  { scopes :: [Map String VarInfo]
  , loopDepth :: Int
  }

type Semantic a = StateT Context L1ExceptT a


semanticAnalysis :: AST -> L1ExceptT ()
semanticAnalysis ast = do
  ctx <- execStateT (varStatusAnalysis ast) (Context [Map.empty] 0)
  execStateT (checkReturns ast) ctx
  return ()

