module Compile.Semantic.LivenessAnalysis
  (
  ) where
  
import           Compile.Semantic.Util
import           Compile.Frontend.AST

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.State
import           Control.Monad (void)


-- TODO
{-
checkLiveness :: AST -> Semantic ()
checkLiveness (Function (Block stmts _)) = do
  scps <- gets scopes
  let initSet = Set.fromList
        [ name
        | scope <- scps
        , (name, VarInfo _ initialized) <- Map.toList scope
        , initialized
        ]
  void $ analyseBlock stmts initSet
  
analyseBlock :: [Stmt] -> Set String -> Semantic (Set String)
analyseBlock [] inSet = return inSet
analyseBlock (s:ss) inSet = do
  checkUses ss inSet
  outSet <- transfer s inSet
  analyseBlock ss nextIn
  -}