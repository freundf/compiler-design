module Compile.IR.Optimize.DeadBlockPrune where

import Compile.IR.IRGraph

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Maybe (fromMaybe)
import Data.List (foldl')

{-pruneUnreachableBlocks :: IRGraph -> IRGraph
pruneUnreachableBlocks ir =
  let
    blocks = filter isBlock . map (getNode ir) . concatMap IntSet.toList . IntMap.elems . successors $ ir


    irSuccWithoutKeys :: IntSet -> IntMap IntSet -> IntMap IntSet
    irSuccWithoutKeys toRemove = \m -> IntMap.withoutKeys m toRemove

isBlock node = case nType node of
  Block _ _ -> True
  _       -> False
-}