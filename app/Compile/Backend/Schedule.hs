module Compile.Backend.Schedule where

import Compile.IR.IRGraph

import           Data.IntMap.Strict    (IntMap)
import qualified Data.IntMap.Strict    as IntMap
import           Data.IntSet  (IntSet)
import qualified Data.IntSet as IntSet
import           Data.Set              (Set)
import qualified Data.Set              as Set
import Data.List (foldl', partition)
import Data.Maybe (mapMaybe)
import Control.Monad.State

type BlockId = Int

data BasicBlock = BasicBlock
  { bid :: BlockId
  , blockNodes :: [Node]
  , blockTerminator :: Node
  } deriving (Show, Eq)
  
instance Ord BasicBlock where
  compare a b = compare (bid a) (bid b)
  
type Schedule = [BasicBlock]

buildBasicBlocks :: IRGraph -> [BasicBlock]
buildBasicBlocks ir = [ BasicBlock blkId blkNodes (head blkTerm)
                      | (blkId, nodesInBlock) <- IntMap.toList (groupNodes block (nodes ir))
                      , let (blkTerm, blkNodes) = partition isTerminator nodesInBlock
                      ]

groupNodes :: (Node -> NodeId) -> IntMap Node -> IntMap [Node]
groupNodes groupingFn = IntMap.foldr insertFn IntMap.empty
  where
    insertFn n acc = IntMap.insertWith (++) (groupingFn n) [n] acc


schedule :: IRGraph -> Schedule
schedule ir = reverse . dfs (Set.toList . blockSuccessors ir basicBlocks) . head $ basicBlocks
  where
    basicBlocks = (buildBasicBlocks ir)

blockSuccessors :: IRGraph -> [BasicBlock] -> BasicBlock -> Set BasicBlock
blockSuccessors ir bbs bb
  | bid bb == endBlock ir = Set.empty
  | otherwise             = let terminator = blockTerminator bb
                                succIds = IntMap.findWithDefault IntSet.empty (nid terminator) (successors ir)
                                succNodes = mapMaybe (resolveProj ir) (IntSet.toList succIds)
                                succBlocks = map (getBasicBlock bbs) succNodes
                            in Set.fromList succBlocks
                                
getBasicBlock :: [BasicBlock] -> Node -> BasicBlock
getBasicBlock bbs node = head . filter ((== block node) . bid) $ bbs
  
resolveProj :: IRGraph -> NodeId -> Maybe Node
resolveProj ir nid = case IntMap.lookup nid (nodes ir) of
  Just n -> case nType n of
                  Proj {}           -> Just . (getNode ir) . head . IntSet.toList $ (successors ir) IntMap.! nid
                  _                 -> Just n
  Nothing   -> Nothing


dfs :: (Ord a) => (a -> [a]) -> a -> [a]
dfs succFn start = evalState (dfsM succFn start) Set.empty

dfsM :: (Ord a) => (a -> [a]) -> a -> State (Set a) [a]
dfsM succFn node = do
  visited <- get
  if Set.member node visited
    then return []
    else do
      modify' (Set.insert node)
      lists <- mapM (dfsM succFn) (succFn node)
      return (concat lists ++ [node])
        