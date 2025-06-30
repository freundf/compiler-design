module Compile.Semantic.FunctionAnalysis
  ( checkFunctions
  ) where


import Compile.Semantic.Util
import Compile.Frontend.AST
import Compile.Semantic.Traverse
import Compile.Semantic.TraversalStates

import Data.List (find, nub)
import Control.Monad (unless, when)
import Control.Monad.State.Strict (get, gets, put)

checkFunctions :: Handler FunctionState
checkFunctions = defaultHandler
  { hAST = checkFuncs
  }


builtins' :: [String]
builtins' = map fName builtins

checkFuncs :: AST -> FunctionState ()
checkFuncs funcs = do
  put (builtins ++ funcs)
  case findMain funcs of
    Nothing -> semanticFail' $ "Missing main method"
    Just m -> checkMain m
  when (any (`elem` builtins') (map fName funcs)) $ semanticFail' $ "Can't redefine builtin functions: " ++ show builtins
  when (length funcs /= length (nub funcs)) $ semanticFail' $ "Can't define the same function twice"

checkMain :: Function -> FunctionState ()
checkMain m = do
  unless ((retType m) == TInt) $ semanticFail' $ "main method must return int"
  unless (null (params m)) $ semanticFail' $ "main method cant take any arguments"


checkCall :: String -> [Expr] -> SourcePos -> FunctionState ()
checkCall f args pos = do
  funcs <- get
  let function = find ((f ==) . fName) funcs
  case function of
    Nothing -> semanticFail' $ "Call to undefined function: " ++ f ++ ", at " ++ show pos
    Just func -> if length (params func) /= length args
                  then semanticFail' $ "Wrong number of arguments for call to '" ++ f ++ "'at " ++ show pos
                  else pure ()

findMain :: AST -> Maybe Function
findMain = find (("main" ==) . fName)
