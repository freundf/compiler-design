module Compile.Semantic.Util
  ( semanticFail'
  , builtins
  ) where

import Compile.Frontend.AST (Function(..), Type(..), posPretty)
import Error (L1ExceptT, MonadCompilerFail, semanticFail)

import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Text.Megaparsec (SourcePos)

-- A little wrapper so we don't have to ($ lift) everywhere inside the StateT
semanticFail' :: MonadCompilerFail m => String -> m a
semanticFail' = semanticFail


builtins :: [Function]
builtins =
  [ Function TInt "print" [(TInt, undefined)] undefined undefined
  , Function TInt "read" [] undefined undefined
  , Function TInt "flush" [] undefined undefined
  ]
