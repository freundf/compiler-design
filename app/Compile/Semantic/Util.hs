module Compile.Semantic.Util
  ( semanticFail'
  , Semantic(..)
  , VarInfo(..)
  , Context(..)
  ) where

import Compile.Frontend.AST (Type)
import Error (L1ExceptT, semanticFail)
import Control.Monad.State (StateT, lift)

import Data.Map (Map)

-- A little wrapper so we don't have to ($ lift) everywhere inside the StateT
semanticFail' :: String -> Semantic a
semanticFail' = lift . semanticFail

type Semantic a = StateT Context L1ExceptT a

data VarInfo = VarInfo
  { vType :: Type
  , vInit :: Bool
  }
  
data Context = Context
  { scopes :: [Map String VarInfo]
  , loopDepth :: Int
  }

