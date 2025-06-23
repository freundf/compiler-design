module Compile.Backend.Schedule where

import Compile.IR.IRGraph

import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.IntMap.Strict    (IntMap)
import qualified Data.IntMap.Strict    as IntMap
import           Data.IntSet  (IntSet)
import qualified Data.IntSet as IntSet
import           Data.Set              (Set)
import qualified Data.Set              as Set
import Data.List (foldl', partition, findIndex)
import Data.Maybe (mapMaybe, fromMaybe)
import Control.Monad.State

import Debug.Trace (traceShow)

type BlockId = Int

data BasicBlock = BasicBlock
  { bid :: BlockId
  , blockNodes :: [Node]
  , blockTerminator :: Node
  } deriving (Show, Eq)
  
instance Ord BasicBlock where
  compare a b = compare (bid a) (bid b)
  
type Schedule = [BasicBlock]

buildBasicBlocks :: IRGraph -> IntMap [Node] -> [NodeId] -> [BasicBlock]
buildBasicBlocks ir groupedNodes blockOrder = reorderBy (\(BasicBlock bid _ _) -> bid) blockOrder
                                            [ BasicBlock bid body (head term)
                                            | (bid, ns) <- IntMap.toList groupedNodes
                                            , let sortedNodes = map (getNode ir) . fullDfs (filter (\n -> block (getNode ir n) == bid) . predecessors . (getNode ir)) . map nid $ ns
                                            , let (term, body) = partition isTerminator sortedNodes
                                            ]

reorderBy :: (Ord k) => (a -> k) -> [k] -> [a] -> [a]
reorderBy keyFn keys values =
  let valMap = Map.fromList [(keyFn v, v) | v <- values]
  in mapMaybe (`Map.lookup` valMap) keys

{-groupNodes :: (Node -> NodeId) -> [Node] -> IntMap [Node]
groupNodes groupingFn nodes = foldl' insertFn IntMap.empty nodes
  where
    insertFn acc n = IntMap.insertWith (++) (groupingFn n) [n] acc
-}


schedule :: IRGraph -> Schedule
schedule ir = let x = buildBasicBlocks ir groupedNodes blockOrder in traceShow x x
  where
    blocks = filter isBlock . IntMap.elems . nodes $ ir
    groupedNodes = IntMap.filterWithKey (\k _ -> k `elem` blockOrder) $ groupNodes ir blocks
    blockOrder = let x = reverse $ dfs (blockSuccs ir) (startBlock ir) in traceShow x x

blockSuccs :: IRGraph -> NodeId -> [NodeId]
blockSuccs ir blkId =
  let term = traceShow ("blockSuccs: blk:" ++ show blkId ++ "term: " ++ show (blockTerm ir blkId)) $ blockTerm ir blkId
  in IntSet.toList . IntSet.fromList . map (block . getNode ir) . foldl' (resolveProj ir) [] $ succs ir term
  where
    resolveProj ir acc n = case (nType (getNode ir n)) of
      Proj {} -> (head $ succs ir n) : acc
      _ -> n : acc

blockTerm :: IRGraph -> NodeId -> NodeId
blockTerm ir blkId = nid . head $ filter (\n -> isTerminator n && block n == blkId) (IntMap.elems (nodes ir))

groupNodes :: IRGraph -> [Node] -> IntMap [Node]
groupNodes ir blocks = go IntSet.empty (map ((blockTerm ir) . nid) blocks) IntMap.empty
  where
    go _ [] acc = acc
    go visited (x:xs) acc
      | x `IntSet.member` visited || isBlock (getNode ir x) = go visited xs acc
      | otherwise = go (IntSet.insert x visited) (xs ++ predecessors (getNode ir x)) (IntMap.insertWith (++) (block node) [node] acc)
      where
        node = getNode ir x

isBlock n = case (nType n) of
  Block {} -> True
  _ -> False


getBasicBlock :: [BasicBlock] -> Node -> BasicBlock
getBasicBlock bbs node = head . filter ((== block node) . bid) $ bbs
  

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

fullDfs :: (Ord a) => (a -> [a]) -> [a] -> [a]
fullDfs succFn starts = evalState (go starts) Set.empty
  where
    go [] = return []
    go (n:ns) = do
      visited <- get
      if Set.member n visited
        then go ns
        else do
          result <- dfsM succFn n
          rest <- go ns
          return (result ++ rest)

