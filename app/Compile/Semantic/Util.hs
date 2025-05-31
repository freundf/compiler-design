module Compile.Semantic.Util
  ( semanticFail'
  , Semantic(..)
  , VarInfo(..)
  , Context
  ) where

-- A little wrapper so we don't have to ($ lift) everywhere inside the StateT
semanticFail' :: String -> Semantic a
semanticFail' = lift . semanticFail

data VarInfo = VarInfo
  { vType :: Type
  , vInit :: Bool
  }
  
data Context = Context
  { scopes :: [Map String VarInfo]
  , loopDepth :: Int
  }

type Semantic a = StateT Context L1ExceptT a
