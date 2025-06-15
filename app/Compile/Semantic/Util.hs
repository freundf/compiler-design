module Compile.Semantic.Util
  ( semanticFail'
  , Semantic(..)
  , Sem
  , VarInfo(..)
  , Context(..)
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

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import Text.Megaparsec (SourcePos)

-- A little wrapper so we don't have to ($ lift) everywhere inside the StateT
semanticFail' :: String -> Semantic a
semanticFail' = lift . semanticFail

type Sem = StateT Context L1ExceptT

type Semantic a = StateT Context L1ExceptT a

data VarInfo = VarInfo
  { vType :: Type
  , vInit :: Bool
  }
  
data Context = Context
  { scopes :: [Map String VarInfo]
  , loopDepth :: Int
  , returnType :: Type
  , recordedTypes :: [Type]
  }

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
      findInScopes (m:ms) = case Map.lookup name m of
                              Just v -> pure v
                              Nothing -> findInScopes ms
  findInScopes (scopes ctx)
      
insertVar :: String -> VarInfo -> SourcePos -> Semantic ()
insertVar name info pos = do
  ctx <- get
  case scopes ctx of
    [] -> semanticFail' $ "Internal error: no scope to insert variable at " ++ posPretty pos
    (cur:rest) ->
      if Map.member name cur
        then semanticFail' $ "Redeclaration of '" ++ name ++ "' at " ++ posPretty pos
        else put ctx { scopes = Map.insert name info cur : rest }

initializeAll :: Semantic ()
initializeAll = do
  scope <- gets (head . scopes)
  let newScope = Map.map (\(VarInfo ty _) -> VarInfo ty True) scope
  modify $ \s -> s { scopes = newScope : (tail (scopes s)) }

updateVar :: String -> VarInfo -> Semantic ()
updateVar name info = do
  scps <- gets scopes
  let update [] = []
      update (m:ms) = if Map.member name m
                    then Map.insert name info m : ms
                    else m : update ms
  modify $ \s -> s { scopes = update scps}

inLoop :: Semantic a -> Semantic a
inLoop m = do
  modify $ \s -> s { loopDepth = loopDepth s + 1}
  res <- m
  modify $ \s -> s { loopDepth = loopDepth s - 1}
  return res

inScope :: Semantic a -> Semantic a
inScope m = do
  ss <- gets scopes
  let newScope = if null ss then Map.empty else head ss
  modify $ \s -> s { scopes = newScope : scopes s}
  res <- m
  modify $ \s -> s { scopes = tail (scopes s) }
  return res
