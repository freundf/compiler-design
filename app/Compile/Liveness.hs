module Compile.Liveness
  ( liveness, livenessGraph
  ) where

import           Compile.X86

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Control.Monad (foldM, when, zipWithM_)
import           Control.Monad.State

type Var = Opnd
type Line = Int

data LivenessState = LivenessState
  { live :: Map Line (Set Var)
  , use :: Map Line (Set Var)
  , def :: Map Line (Set Var)
  , succs :: Map Line (Set Line)
  } deriving (Show)
  
type LivenessAnalysis a = State LivenessState a

liveness :: X86 -> LivenessState
liveness instrs = execState (extractFacts (reverse instrs) >> livenessAnalysis) initialState
  where
    initialState = LivenessState Map.empty Map.empty Map.empty Map.empty
    
    


extractFacts :: [Instr] -> LivenessAnalysis ()
extractFacts instr = zipWithM_ processInstr instr [n, n - 1 .. 1]
  where
    n = length instr

instrFacts :: Instr -> (Set Var, Set Var, Line -> Set Line)
instrFacts instr = liftToSet $ case instr of
  --  instr ->  ( use,                   def,                succs)
  Mov o1 o2 ->  ( [o2],                  [o1],               [(+1)] )
  Add o1 o2 ->  ( [o1, o2],              [o1],               [(+1)] )
  Sub o1 o2 ->  ( [o1, o2],              [o1],               [(+1)] )
  Imul o1 o2 -> ( [o1, o2],              [o1],               [(+1)] )
  Idiv o ->     ( [o, Reg RAX, Reg RDX], [Reg RAX, Reg RDX], [(+1)] )
  Cdq ->        ( [Reg RAX],             [Reg RDX],          [(+1)] )
  Neg o ->      ( [o],                   [o],                [(+1)] )
  Ret ->        ( [Reg RAX],             [],                 []     )
  _ ->          ( [],                    [],                 [(+1)] )
  where
    liftToSet :: ([Var], [Var], [Line -> Line]) -> (Set Var, Set Var, Line -> Set Line)
    liftToSet (u, d, s) = ( Set.fromList $ filter isVar u
                          , Set.fromList $ filter isVar d
                          , Set.fromList . (s <*>) . pure
                          )
                          
    isVar (Imm _) = False
    isVar _       = True
                          
processInstr :: Instr -> Line -> LivenessAnalysis ()
processInstr instr l = do
  let (useVars, defVars, succLines) = instrFacts instr
  modify $ \s -> s
    { use   = Map.insertWith Set.union l useVars (use s)
    , def   = Map.insertWith Set.union l defVars (def s)
    , succs = Map.insertWith Set.union l (succLines l) (succs s)
    }

livenessAnalysis :: LivenessAnalysis ()
livenessAnalysis = do
  changed <- livenessStep
  when changed livenessAnalysis
  
livenessStep :: LivenessAnalysis Bool
livenessStep = do
  st <- get
  let ls = Map.keys (use st)
  foldM updateLine False ls
  
updateLine :: Bool -> Line -> LivenessAnalysis Bool
updateLine acc l = do
  st <- get
  let used = find l (use st)
      defined = find l (def st)
      successors = find l (succs st)
      live_k1 = used
      live_k2 = Set.fromList [ u
                             | l' <- Set.toList successors
                             , u <- Set.toList (find l' (live st))
                             , Set.notMember u defined
                             ]
      newLive = Set.union live_k1 live_k2
      changed = newLive /= (find l (live st))
   
  modify $ \s -> s { live = Map.insertWith Set.union l newLive (live st) }
  pure (acc || changed)
  
-- helper function
find :: Ord k => k -> Map k (Set v) -> (Set v)
find k m = Map.findWithDefault Set.empty k m
  
livenessGraph :: LivenessState -> Map Var (Set Var)
livenessGraph ls =
  let
    livePairs =
      [ interferencePairs vars
      | vars <- Map.elems (live ls)
      ]
    
    allVars = Set.unions (Map.elems (use ls) ++ Map.elems (def ls))
    allNodes = Map.fromSet (const Set.empty) allVars
  in Map.unionWith Set.union allNodes (Map.unionsWith Set.union livePairs)
    where
      interferencePairs :: Set Var -> Map Var (Set Var)
      interferencePairs vars =
        Map.fromListWith Set.union
          [ (v1, Set.singleton v2)
          | v1 <- Set.toList vars
          , v2 <- Set.toList vars
          , v1 /= v2
          ]