module Compile.IR.ControlFlow where

import Compile.IR.IRGraph

import           Data.IntMap.Strict    (IntMap)
import qualified Data.IntMap.Strict    as IntMap
import           Data.IntSet           (IntSet)
import qualified Data.IntSet           as IntSet
import           Data.List             (partition, sortOn, foldl')
import           Data.Maybe            (fromMaybe)
import           Data.Set              (Set)
import qualified Data.Set              as Set


type BlockId = Int

data BasicBlock = BasicBlock
  { bid :: BlockId
  , bNodes :: [Node]
  , terminator :: Node
  } deriving (Eq, Show)

data ControlFlowGraph = ControlFlowGraph
  { blocks :: IntMap BasicBlock
  , cfgNodes :: IntMap Node
  , order :: [BasicBlock]
  , entryBlock :: BlockId
  , exitBlock :: BlockId
  } deriving (Eq, Show)
  

buildCFG :: IRGraph -> ControlFlowGraph
buildCFG graph = ControlFlowGraph
  { blocks = blocksMap
  , cfgNodes = nodes graph
  , order = orderedBlocks
  , entryBlock = startBlock graph
  , exitBlock = endBlock graph
  }
  where
    byBlock = groupNodes (nodes graph)
    blocksMap = IntMap.mapWithKey buildBasicBlock byBlock
    
    orderedBids = orderBlocks graph
    lookupBB bId = fromMaybe
      (error $ "buildCFG: orderBlocks returned unknown BlockId " ++ show bId)
      (IntMap.lookup bId blocksMap)
    orderedBlocks = map lookupBB orderedBids
    
  
  
groupNodes :: IntMap Node -> IntMap [Node]
groupNodes = IntMap.filter (not . (== 1) . length) . IntMap.foldr insertFn IntMap.empty
  where
    insertFn n acc = IntMap.insertWith (++) (block n) [n] acc
    
buildBasicBlock :: BlockId -> [Node] -> BasicBlock
buildBasicBlock bid allNodes = case partition isTerminator sortedNodes of
  ([termNode], rest) -> BasicBlock bid rest termNode
  ([], _) -> error $ "buildCFG: Block " ++ show bid ++ " has no terminator!\nNodes: " ++ show sortedNodes
  (_:_:_, _) -> error $ "buildCFG Block" ++ show bid ++ " has multiple terminators!\nNodes: " ++ show sortedNodes
  where
    sortedNodes = sortOn nid allNodes
    
orderBlocks :: IRGraph -> [BlockId]
orderBlocks graph =
  let blocksList = getBlocks graph
      succsFunc bId
          | bId `elem` blocksList = let nodeSuccs = IntSet.toList (IntMap.findWithDefault IntSet.empty bId (successors graph))
                                        blkSuccs  = Set.fromList
                                          [ block
                                            (IntMap.findWithDefault
                                              (error "orderBlocks: unknown node")
                                              nid'
                                              (nodes graph)
                                            )
                                          | nid' <- nodeSuccs
                                          ]
                                    in  Set.toList blkSuccs
          | otherwise = []
  
      start = startBlock graph
      reachable = reverse $ dfsPostorder succsFunc start
      unreachable = filter (`notElem` reachable) blocksList
  in reachable ++ unreachable
  
  
dfsPostorder :: (Ord a) => (a -> [a]) -> a -> [a]
dfsPostorder f start = dfs' start Set.empty []
  where
    dfs' n visited acc
      | n `Set.member` visited  = acc
      | otherwise =
        let seen = Set.insert n visited
            succs = f n
            acc' = foldl' (\a v -> dfs' v seen a) acc succs
        in n : acc'