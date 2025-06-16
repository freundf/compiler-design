module Compile.IR.Optimize.CleanupOptimizer where

import Compile.IR.IRGraph

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

cleanup :: IRGraph -> IRGraph
cleanup graph =
  let isDeadConst :: Node -> Bool
      isDeadConst node = case nType node of
        ConstNode {} -> not (nid node `IntMap.member` (successors graph))
        _ -> False
       
        
      newNodes = IntMap.filter (not . isDeadConst) (nodes graph)
      in graph { nodes = newNodes }