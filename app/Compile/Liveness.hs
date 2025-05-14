module Compile.Liveness
  ( liveness, livenessGraph
  ) where

import           Compile.X86

import           Prelude hiding (succ)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Control.Monad (foldM, when, zipWithM_)
import           Control.Monad.State

type Var = Opnd
type Line = Integer

data LivenessState = LivenessState
  { live :: Map Line (Set Var)
  , use :: Map Line (Set Var)
  , def :: Map Line (Set Var)
  , succ :: Map Line (Set Line)
  } deriving (Show)
  
addSucc :: Line -> Line -> LivenessState -> LivenessState
addSucc l succL = \s -> s { succ = Map.insertWith Set.union l (Set.singleton succL) (succ s) }
  
addDef :: Line -> Var -> LivenessState -> LivenessState
addDef l v = \s -> s { def = Map.insertWith Set.union l (Set.singleton v) (def s) }

addUse :: Line -> Var -> LivenessState -> LivenessState
addUse l v = \s -> s { use = Map.insertWith Set.union l (Set.singleton v) (use s) }

type LivenessAnalysis a = State LivenessState a

liveness :: X86 -> LivenessState
liveness instrs = execState (extractFacts (reverse instrs) >> livenessAnalysis) initialState
  where
    initialState = LivenessState Map.empty Map.empty Map.empty Map.empty
    

extractFacts :: [Instr] -> LivenessAnalysis ()
extractFacts instr = zipWithM_ processInstr instr [1..]

processBinOp :: Opnd -> Opnd -> Line -> LivenessAnalysis ()
processBinOp o1 o2 l = do
  case o1 of
    (Imm _) -> pure ()
    _ -> do
      modify $ addDef l o1
      modify $ addUse l o1
  case o2 of
    (Imm _) -> pure ()
    _ -> do
      modify $ addUse l o2
  modify $ addSucc l (l + 1)

processInstr :: Instr -> Line -> LivenessAnalysis ()
processInstr (Mov o1 o2) l = do
  case o1 of
    (Imm _) -> pure ()
    _ -> do
      modify $ addDef l o1
  case o2 of
    (Imm _) -> pure ()
    _ -> do
      modify $ addUse l o2
  modify $ addSucc l (l + 1)
processInstr (Add o1 o2) l = processBinOp o1 o2 l
processInstr (Sub o1 o2) l = processBinOp o1 o2 l
processInstr (Imul o1 o2) l = processBinOp o1 o2 l
processInstr (Idiv o) l = do
  case o of
    (Imm _) -> pure ()
    _ -> do
      modify $ addUse l o
  modify $ addUse l (Reg RAX)
  modify $ addUse l (Reg RDX)
  modify $ addDef l (Reg RAX)
  modify $ addDef l (Reg RDX)
  modify $ addSucc l (l + 1)
processInstr (Cdq) l = do
  modify $ addUse l (Reg RAX)
  modify $ addDef l (Reg RDX)
  modify $ addSucc l (l + 1)
processInstr (Neg o) l = do
  modify $ addUse l o
  modify $ addDef l o
  modify $ addSucc l (l + 1)
processInstr (Ret) l = do
  modify $ addUse l (Reg RAX)
  modify $ addSucc l (l + 1)
processInstr _ l = modify $ addSucc l (l + 1)


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
      succs = find l (succ st)
      live_k1 = used
      live_k2 = Set.fromList [ u
                             | l' <- Set.toList succs
                             , u <- Set.toList (find l' (live st))
                             , Set.notMember u defined
                             ]
      newLive = Map.insertWith Set.union l (Set.union live_k1 live_k2) (live st)
      changed = newLive /= (live st)
   
  modify $ \s -> s { live = newLive }
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