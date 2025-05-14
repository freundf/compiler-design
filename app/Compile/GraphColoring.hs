module Compile.GraphColoring
  ( colorGraph
  ) where
  
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Ord (comparing)
import           Data.List (maximumBy)

type InterferenceGraph a = Map a (Set a)

colorGraph :: (Ord a, Ord c) => [c] -> InterferenceGraph a -> Map a c
colorGraph colors graph = greedyColoring colors graph (maximumCardinalitySearch graph)


greedyColoring :: (Ord a, Ord c) => [c] -> InterferenceGraph a -> [a] -> Map a c
greedyColoring colors graph ordering = greedy ordering Map.empty
  where
    greedy [] coloring = coloring
    greedy (n:ns) coloring = greedy ns (Map.insert n (chooseColor n coloring) coloring)
    
    chooseColor n coloring = head [c | c <- colors, c `Set.notMember` neighbourColors n coloring]
 
    neighbourColors n coloring = Set.fromList [c | v <- Set.toList (neighbours n)
                                                 , Just c <- [Map.lookup v coloring]]
    neighbours n = Map.findWithDefault Set.empty n graph


maximumCardinalitySearch :: (Ord a) => InterferenceGraph a -> [a]
maximumCardinalitySearch graph = mcs graph (Map.map (const 0) graph) (Set.fromList (Map.keys graph)) []
  where
    mcs _ _ remaining ordering | Set.null remaining = ordering
    mcs graph weights remaining ordering =
      let v = maximumBy (comparing (weights Map.!)) (Set.toList remaining)
          neighbours = Map.findWithDefault Set.empty v graph
          newWeights = foldr (\u -> Map.adjust (+1) u) weights (Set.toList neighbours)
      in mcs graph newWeights (Set.delete v remaining) (ordering ++ [v])
      
      
exampleGraph = Map.fromList
  [(1,Set.fromList [2,3,4,5])
  ,(2,Set.fromList [1,3,4])
  ,(3,Set.fromList [1,2,4,5])
  ,(4,Set.fromList [2,3])
  ,(5,Set.fromList [3,2,3])
  ,(6,Set.fromList [1,2,3,4,5])
  ,(7,Set.fromList [3,4,5,1,6])
  ,(8,Set.fromList [1,2,3,4,5,6,7])
  ]
  