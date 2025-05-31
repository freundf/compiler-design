module Compile.Semantic.LivenessAnalysis
  ( checkLiveness
  ) where
  
import           Compile.Semantic.Util

import           Data.Set (Set)
import qualified Data.Set as Set


checkLiveness :: AST -> Semantic ()
checkLiveness (Program (Block stmts _)) = do
  scps <- gets scopes
  let initSet = Set.fromList
    [ name
    | scope <- scps
    , (name, VarInfo _ initialized) <- Map.toList scope
    , initialized
    ]
  void $ analyzeBlock stmts initSet
  
analyzeBlock :: [Stmt] -> Set String -> Sem (Set String)
analyzeBlock [] inSet = return inSet
analyzeBlock (s:ss) inSet = do
  checkUses ss inSet
  outSet <- transfer s inSet
  analyzeBlock ss nextIn
  