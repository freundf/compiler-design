module Compile.Semantic.FunctionAnalysis
  ( checkFunctions
  ) where


import Compile.Semantic.Util
import Compile.Frontend.AST
import Compile.Semantic.Traverse

import Data.List (find, nub)
import Control.Monad (unless, when)

checkFunctions :: Handler Sem
checkFunctions = defaultHandler
  { hAST = checkFuncs
  }


builtins' :: [String]
builtins' = map fName builtins

checkFuncs :: AST -> Semantic ()
checkFuncs funcs = do
  case findMain funcs of
    Nothing -> semanticFail' $ "Missing main method"
    Just m -> checkMain m
  when (any (`elem` builtins') (map fName funcs)) $ semanticFail' $ "Can't redefine builtin functions: " ++ show builtins
  when (length funcs /= length (nub funcs)) $ semanticFail' $ "Can't define the same function twice"

checkMain :: Function -> Semantic ()
checkMain m = do
  unless ((retType m) == TInt) $ semanticFail' $ "main method must return int"
  unless (null (params m)) $ semanticFail' $ "main method cant take any arguments"


findMain :: AST -> Maybe Function
findMain = find (("main" ==) . fName)
