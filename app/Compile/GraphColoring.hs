module Compile.GraphColoring
  (
  ) where
  
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Ord (comparing)
import           Data.List (maximumBy)

type InterferenceGraph = Map Integer (Set Integer)

colorGraph :: InterferenceGraph -> Map Integer Integer
colorGraph graph = greedyColoring graph (maximumCardinalitySearch graph)


greedyColoring :: InterferenceGraph -> [Integer] -> Map Integer Integer
greedyColoring graph ordering = greedy graph ordering Map.empty
  where
    greedy graph [] coloring = coloring
    greedy graph (n:ns) coloring = greedy graph ns (Map.insert n (color n) coloring)
      where
        color n = findMinColor (neighbourColors n) 0
        neighbours n = Map.findWithDefault Set.empty n graph
        neighbourColors n = Set.map (\x -> Map.findWithDefault (-1) x coloring) (neighbours n)
        findMinColor s color = case Set.minView s of
          Nothing -> color
          Just (y, ys) -> if Set.member color s
                          then findMinColor (Set.delete color s) (color + 1)
                          else color


maximumCardinalitySearch :: InterferenceGraph -> [Integer]
maximumCardinalitySearch graph = mcs graph (Map.map (const 0) graph) (Set.fromList (Map.keys graph)) []
  where
    mcs :: InterferenceGraph -> Map Integer Int -> Set Integer -> [Integer] -> [Integer]
    mcs _ _ remaining ordering | Set.null remaining = ordering
    mcs graph weights remaining ordering =
      let v = maximumBy (comparing (weights Map.!)) (Set.toList remaining)
          newWeights = foldr (\u -> Map.adjust (+1) u) weights (Set.toList (Map.findWithDefault Set.empty v graph))
      in mcs graph newWeights (Set.delete v remaining) (ordering ++ [v])
      
      
exampleGraph = Map.fromList
  [(1,Set.fromList [2,3])
  ,(2,Set.fromList [1,3,4])
  ,(3,Set.fromList [1,2,4,5])
  ,(4,Set.fromList [2,3])
  ,(5,Set.fromList [3])
  ]
  