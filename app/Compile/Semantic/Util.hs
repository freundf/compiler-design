module Compile.Semantic.Util
  ( semanticFail'
  , Semantic(..)
  , Sem
  , VarInfo(..)
  , Context(..)
  , Scope(..)
  , ScopeType(..)
  , popTypes
  , pushType
  , insertVar
  , lookupVar
  , updateVar
  , inScope
  , inLoop
  , initializeAll
  ) where

import Compile.Frontend.AST (Type, posPretty)
import Error (L1ExceptT, semanticFail)

import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Text.Megaparsec (SourcePos)

-- A little wrapper so we don't have to ($ lift) everywhere inside the StateT
semanticFail' :: String -> Semantic a
semanticFail' = lift . semanticFail

type Sem = StateT Context L1ExceptT

type Semantic a = StateT Context L1ExceptT a

data VarInfo = VarInfo
  { vType :: Type
  , vInit :: Bool
  } deriving (Eq, Show)
  
data Context = Context
  { scopes :: [Scope]
  , oldScopes :: [Scope]
  , loopDepth :: Int
  , returnType :: Type
  , recordedTypes :: [Type]
  } deriving (Eq, Show)

data Scope = Scope
  { vars :: Map String VarInfo
  , sType :: ScopeType
  } deriving (Eq, Show)

data ScopeType = Transparent | Opaque
  deriving (Eq, Show)

popTypes :: Int -> Semantic [Type]
popTypes x = do
  types <- gets recordedTypes
  modify $ \s -> s { recordedTypes = drop x types }
  return (take x types)

pushType :: Type -> Semantic ()
pushType t = modify $ \s -> s { recordedTypes = t : recordedTypes s }

lookupVar :: String -> SourcePos -> Semantic VarInfo
lookupVar name pos = do
  ctx <- get
  let findInScopes [] = semanticFail' ("Use of undeclared '" ++ name ++ "' at " ++ posPretty pos)
      findInScopes ((Scope cur ty):ss) = case Map.lookup name cur of
                              Just v -> pure v
                              Nothing -> findInScopes ss
  findInScopes (scopes ctx)
      
insertVar :: String -> VarInfo -> SourcePos -> Semantic ()
insertVar name info pos = do
  ctx <- get
  case scopes ctx of
    [] -> semanticFail' $ "Internal error: no scope to insert variable at " ++ posPretty pos
    ((Scope cur ty) : rest) ->
      if Map.member name cur
        then semanticFail' $ "Redeclaration of '" ++ name ++ "' at " ++ posPretty pos
        else put ctx { scopes = (Scope (Map.insert name info cur) ty) : rest }

initializeAll :: Semantic ()
initializeAll = do
  scope <- gets (head . scopes)
  let newScope = scope { vars = Map.map (\(VarInfo ty _) -> VarInfo ty True) (vars scope) }
  modify $ \s -> s { scopes = newScope : (tail (scopes s)) }

updateVar :: String -> VarInfo -> Semantic ()
updateVar name info = do
  scps <- gets scopes
  let update [] = []
      update (s:ss)
        | sType s == Opaque = if Map.member name (vars s)
                                then (Scope (Map.insert name info (vars s)) Opaque) : ss
                                else s : ss
        | otherwise         = if Map.member name (vars s)
                                then (Scope (Map.insert name info (vars s)) Transparent) : update ss
                                else s : update ss
  modify $ \s -> s { scopes = update scps}

inLoop :: Semantic a -> Semantic a
inLoop m = inScope Opaque $ do
  modify $ \s -> s { loopDepth = loopDepth s + 1}
  res <- m
  modify $ \s -> s { loopDepth = loopDepth s - 1}
  return res

inScope :: ScopeType -> Semantic a -> Semantic a
inScope ty m = do
  ss <- gets scopes
  let newScope = if null ss
                  then Scope Map.empty ty
                  else Scope (vars (head ss)) ty
  modify $ \s -> s { scopes = newScope : scopes s}
  res <- m
  modify $ \s -> s { scopes = tail (scopes s), oldScopes = head (scopes s) : (oldScopes s) }
  return res
