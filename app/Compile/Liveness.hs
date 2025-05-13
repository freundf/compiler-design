module Liveness
  (
  ) where

import           Compile.X86

import           Prelude hiding (succ)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Control.Monad.State
import           Data.Maybe (fromJust)
import           Data.Either (fromRight)

type Var = Integer
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

addLive :: Line -> Var -> LivenessState -> LivenessState
addLive l v = \s -> s { live = Map.insertWith Set.union l (Set.singleton v) (live s) }

type LivenessAnalysis a = State LivenessState a

liveness :: X86 -> LivenessState
liveness instrs = execState (extractFacts instrs >> livenessAnalysis) initialState
  where
    initialState = LivenessState Map.empty Map.empty Map.empty Map.empty

extractFacts :: [Instr] -> LivenessAnalysis ()
extractFacts instr = zipWithM_ processInstr instr [1..]

processBinOp :: Opnd -> Opnd -> Line -> LivenessAnalysis ()
processBinOp o1 o2 l = do
  case o1 of
    VirtReg i -> do
      modify $ addDef l i
      modify $ addUse l i
    _         -> pure ()
  case o2 of
    VirtReg i -> modify $ addUse l i
    _         -> pure ()
  modify $ addSucc l (l + 1)


processInstr :: Instr -> Line -> LivenessAnalysis ()
processInstr (Mov o1 o2) l = do
  case o1 of
    VirtReg i -> modify $ addDef l i
    _         -> pure ()
  case o2 of
    VirtReg i -> modify $ addUse l i
    _         -> pure ()
  modify $ addSucc l (l + 1)
processInstr (Add o1 o2) l = processBinOp o1 o2 l
processInstr (Sub o1 o2) l = processBinOp o1 o2 l
processInstr (Imul o1 o2) l = processBinOp o1 o2 l
processInstr (Idiv o) l = do
  case o of
    VirtReg i -> modify $ addUse l i
    _         -> pure ()
  modify $ addSucc l (l + 1)
processInstr (Neg o) l = do
  case o of
    VirtReg i -> do
      modify $ addUse l i
      modify $ addDef l i
    _         -> pure ()
  modify $ addSucc l (l + 1)
processInstr (Ret) _ = pure ()
processInstr _ l = modify $ addSucc l (l + 1)


livenessAnalysis :: LivenessAnalysis ()
livenessAnalysis = do
  changed <- livenessStep
  when changed livenessAnalysis
  
livenessStep :: LivenessAnalysis Bool
livenessStep = do
  st <- get
  let lines = Map.keys (use st)
  foldM updateLine False lines
  
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
  Map.unionsWith Set.union $
      [ interferencePairs vars
      | vars <- Map.elems (live ls)
      ]
    where
      interferencePairs :: Set Var -> Map Var (Set Var)
      interferencePairs vars =
        Map.fromListWith Set.union
          [ (v1, Set.singleton v2)
          | v1 <- Set.toList vars
          , v2 <- Set.toList vars
          , v1 /= v2
          ]
  
example_live :: X86
example_live = [
  Mov (VirtReg 1) (Imm "1"),
  Mov (VirtReg 2) (VirtReg 1),
  Add (VirtReg 2) (VirtReg 1),
  Mov (VirtReg 3) (VirtReg 2),
  Add (VirtReg 3) (VirtReg 2),
  Mov (VirtReg 4) (VirtReg 1),
  Add (VirtReg 4) (VirtReg 2),
  Mov (VirtReg 5) (VirtReg 4),
  Add (VirtReg 5) (VirtReg 3),
  Mov (Reg RAX) (VirtReg 5),
  Ret
  ]
  

