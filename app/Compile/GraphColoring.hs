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

colorGraph :: (Show a, Show c, Ord a, Ord c) => [c] -> InterferenceGraph a -> Map a c -> Map a c
colorGraph colors graph precolored = greedyColoring colors graph (maximumCardinalitySearch graph) precolored

greedyColoring :: (Ord a, Ord c) => [c] -> InterferenceGraph a -> [a] -> Map a c -> Map a c
greedyColoring colors graph ordering precolored = greedy ordering precolored
  where
    greedy [] coloring = coloring
    greedy (n:ns) coloring
      | Map.member n coloring = greedy ns coloring
      | otherwise =
          let color = chooseColor n coloring
          in greedy ns (Map.insert n color coloring)
    
    chooseColor n coloring = head [c | c <- colors, c `Set.notMember` neighbourColors n coloring]
 
    neighbourColors n coloring = Set.fromList [c | v <- Set.toList (neighbours n)
                                                 , Just c <- [Map.lookup v coloring]]
    neighbours n = Map.findWithDefault Set.empty n graph


maximumCardinalitySearch :: (Ord a) => InterferenceGraph a -> [a]
maximumCardinalitySearch graph = mcs (Map.map (const (0 :: Int)) graph) (Set.fromList (Map.keys graph)) []
  where
    mcs _ remaining ordering | Set.null remaining = ordering
    mcs weights remaining ordering =
      let v = maximumBy (comparing (weights Map.!)) (Set.toList remaining)
          neighbours = Map.findWithDefault Set.empty v graph
          newWeights = foldr (\u -> Map.adjust (+1) u) weights (Set.toList neighbours)
      in mcs newWeights (Set.delete v remaining) (ordering ++ [v])
      