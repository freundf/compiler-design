module Compile.Backend.Asm
  ( codeGen
  ) where
  
import           Compile.Backend.X86
import           Compile.IR.IR (IRGraph(..), Node(..), NodeId, topoSort)
import qualified Compile.IR.IR as IR
import           Compile.IR.RegAlloc (regAlloc, coloringStrategy, naiveStrategy)

import           Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.IntSet  (IntSet)
import qualified Data.IntSet as IntSet

type CodeGen a = State CodeGenState a

data CodeGenState = CodeGenState
  { regMap :: Map NodeId Opnd
  , irGraph :: IRGraph
  , nextReg :: Integer
  , code :: X86
  }

codeGen :: IRGraph -> X86
codeGen graph = Prologue : regAlloc (code finalState) strategy
  where
    initialState = CodeGenState Map.empty graph 0 []
    order = topoSort graph
    finalState = execState (mapM_ genNode order) initialState
    strategy = let maxReg = nextReg (finalState) - 1
               in naiveStrategy maxReg
    
freshReg :: CodeGen Opnd
freshReg = do
  n <- gets nextReg
  modify $ \s -> s {nextReg = n + 1}
  pure (VirtReg n)
  
recordResult :: NodeId -> Opnd -> CodeGen ()
recordResult nid r = modify $ \s -> s { regMap = Map.insert nid r (regMap s)}
  
lookupResult :: NodeId -> CodeGen Opnd
lookupResult n = do
  m <- gets regMap
  case Map.lookup n m of
    Just r -> pure r
    Nothing -> error "unreachable, error in semantic analysis"
    
emit :: Instr -> CodeGen()
emit instr = modify $ \s -> s { code = (code s) ++ [instr]}

node :: NodeId -> CodeGen (Node)
node n = (IntMap.! n) . irNodes . irGraph <$> get

genNode :: NodeId -> CodeGen Opnd
genNode n = do
  node <- node n
  case node of
    Start _ _ -> pure (Reg EAX)
    
    Const _ i _ -> do
      r <- freshReg
      emit $ Mov r (Imm i)
      recordResult n r
      pure r
      
    UnaryOp _ IR.Neg e _ -> do
      r1 <- lookupResult e
      r2 <- freshReg
      emit $ Mov r2 r1
      emit $ Neg r2
      recordResult n r2
      pure r2
      
    BinaryOp _ op lhs rhs _ _ -> do
      r1 <- lookupResult lhs
      r2 <- lookupResult rhs
      r3 <- freshReg
      case op of
        IR.Add -> emit (Mov r3 r1) >> emit (Add r3 r2)
        IR.Mul -> emit (Mov r3 r1) >> emit (Imul r3 r2)
        IR.Sub -> emit (Mov r3 r1) >> emit (Sub r3 r2)
        IR.Div -> do
          emit $ Mov (Reg EAX) r1
          emit $ Cdq
          emit $ Idiv r2
          emit $ Mov r3 (Reg EAX)
        IR.Mod -> do
          emit $ Mov (Reg EAX) r1
          emit $ Cdq
          emit $ Idiv r2
          emit $ Mov r3 (Reg EDX)
        _ -> error ("unknown binary operator: " ++ show op)
      recordResult n r3
      pure r3
      
    Return _ e _ _ -> do
      r <- lookupResult e
      emit $ Mov (Reg EAX) r
      emit $ Ret
      pure r
    
    _ -> error ("unknown node: " ++ show node)
