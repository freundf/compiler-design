{-# LANGUAGE FlexibleInstances #-}
module Compile.Semantic.TraversalStates where

import Compile.Semantic.Traverse
import Error
import Compile.Frontend.AST (Function(..), Type(..), posPretty)

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict

import Text.Megaparsec (SourcePos)

-- Does nothing
type NoState = StateT () L1ExceptT

emptyNoState = ()

instance TraverseMonad NoState where
  inLoop m = m
  inScope m = m
  inScope_ m = m

-- Counts loop depth
type LoopState = StateT Integer L1ExceptT

emptyLoopState = 0

instance TraverseMonad LoopState where
  inLoop m = inScope $ do
    n <- get
    res <- m
    put (n + 1)
    return res

  inScope m = m
  inScope_ m = m


-- Tracks variables
type VariableState = StateT VarState L1ExceptT

emptyVariableState = VarState [] [] []

data VarState = VarState
  { scopes :: [Scope]
  , scopeIf :: [Scope]
  , scopeElse :: [Scope]
  } deriving (Eq, Show)

data Scope = Scope
  { declarations :: Set String
  , definitions :: Set String
  } deriving (Eq, Show)

enterScope :: VariableState ()
enterScope = modify $ \s -> s { scopes = (Scope Set.empty Set.empty) : (scopes s) }

exitScope :: VariableState ()
exitScope = do
  ss <- gets scopes
  if length ss == 1
  then pure ()
  else do
    let cur = head ss
        prev = head (tail ss)
        decls = declarations cur
        defs = decls Set.\\ (definitions cur)
        updated = prev { definitions = defs `Set.union` (definitions prev) }
    modify $ \s -> s { scopes = updated : (drop 2 (scopes s)) }

exitScope_ :: VariableState ()
exitScope_ = modify $ \s -> s { scopes = tail (scopes s) }

declare :: String -> SourcePos -> VariableState ()
declare name pos = do
  decl <- getDeclaration name
  case decl of
    Just _ -> semanticFail $ "Redeclaration of '" ++ name ++ "' at " ++ posPretty pos
    Nothing -> do
      scope <- gets (head . scopes)
      let updated = scope { declarations = Set.insert name (declarations scope) }
      modify $ \s -> s { scopes = updated : (tail (scopes s)) }

define :: String -> SourcePos -> VariableState ()
define name pos = do
  decl <- getDeclaration name
  case decl of
    Nothing -> semanticFail $ "Define: Use of undeclared '" ++ name ++ "' at " ++ posPretty pos
    Just _ -> do
      scope <- gets (head . scopes)
      let updated = scope { definitions = Set.insert name (definitions scope) }
      modify $ \s -> s { scopes = updated : (tail (scopes s)) }

check :: String -> SourcePos -> VariableState ()
check name pos = do
  decl <- getDeclaration name
  def <- getDefinition name
  case decl of
    Nothing -> semanticFail $ "Check: Use of undeclared '" ++ name ++ "' at " ++ posPretty pos
    Just _ -> pure ()
  case def of
    Nothing -> semanticFail $ "Check: Use of undefined '" ++ name ++ "' at " ++ posPretty pos
    Just _ -> pure ()

getDeclaration :: String -> VariableState (Maybe String)
getDeclaration name = do
  ss <- gets (map declarations . scopes)
  if any (Set.member name) ss
    then pure $ Just name
    else pure $ Nothing

getDefinition :: String -> VariableState (Maybe String)
getDefinition name = do
  ss <- gets (map definitions . scopes)
  if any (Set.member name) ss
    then pure $ Just name
    else pure $ Nothing

initializeAll :: VariableState ()
initializeAll = do
  scope <- gets (head . scopes)
  let updated = scope { definitions = Set.union (declarations scope) (definitions scope) }
  modify $ \s -> s { scopes = updated : (tail (scopes s)) }

getCurrentScope :: VariableState Scope
getCurrentScope = do
  scope <- gets (head . scopes)
  return scope


instance TraverseMonad VariableState where
  inScope m = do
    enterScope
    res <- m
    exitScope
    return res

  inScope_ m = do
    enterScope
    res <- m
    exitScope_
    return res
  inLoop m = inScope_ m


-- Tracks functions
type FunctionState = StateT [Function] L1ExceptT

emptyFunctionState = []

instance TraverseMonad FunctionState where
  inLoop m = m
  inScope m = m
  inScope_ m = m


-- Tracks Variable Types
type TypeState = StateT Types L1ExceptT

emptyTypeState = Types [Map.empty] [] TAny []

data Types = Types
  { types :: [Map String Type]
  , recordedTypes :: [Type]
  , returnType :: Type
  , functions :: [Function]
  } deriving (Eq, Show)

registerReturnType :: Type -> TypeState ()
registerReturnType t = modify $ \s -> s { returnType = t }

registerType :: String -> Type -> TypeState ()
registerType name ty = do
  t <- gets (head . types)
  let updated = Map.insert name ty t
  modify $ \s -> s { types = updated : (tail (types s)) }

getType :: String -> SourcePos -> TypeState Type
getType name pos = do
  ts <- gets (head . types)
  let t = Map.lookup name ts
  case t of
    Nothing -> semanticFail $ "getType: Use of undeclared '" ++ name ++ "' at " ++ posPretty pos
    Just t -> pure t

popTypes :: Int -> TypeState [Type]
popTypes x = do
  types <- gets recordedTypes
  modify $ \s -> s { recordedTypes = drop x types }
  return (take x types)

pushType :: Type -> TypeState ()
pushType t = modify $ \s -> s { recordedTypes = t : recordedTypes s }

instance TraverseMonad TypeState where
  inLoop m = inScope m
  inScope m = do
    modify $ \s -> s { types = (head (types s)) : (types s) }
    res <- m
    modify $ \s -> s { types = tail (types s) }
    return res
  inScope_ m = inScope m
