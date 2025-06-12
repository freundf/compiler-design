module Compile.Backend.Asm
  ( codeGen
  ) where
  
import           Compile.Backend.X86.X86
import           Compile.Backend.X86.Register
import           Compile.Backend.X86.Instruction
import           Compile.Backend.Schedule
import           Compile.IR.IRGraph hiding (BinOp(..), UnOp(..))
import qualified Compile.IR.IRGraph as IR
import Compile.IR.RegAlloc

import           Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.IntSet  (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (findIndex)
import Data.Maybe (fromJust)

import Control.Monad.IO.Class

type CodeGen a = State CodeGenState a

data CodeGenState = CodeGenState
  { regMap :: Map NodeId Opnd
  , order :: Schedule
  , irGraph :: IRGraph
  , nextReg :: Integer
  , currentBlock :: Int
  , codeBlocks :: IntMap [Instr]
  }
  
codeGen :: IRGraph -> X86
codeGen ir = regAlloc ((X86 defaultDirectives) . (prologue ++) . concatMap flattenBlock $ schedule ir) strategy
  where
    order = schedule ir
    initialState = CodeGenState Map.empty order ir 0 0 IntMap.empty
    genBlocks = do
      mapM_ genBlock order
      mapM_ genTerminator order
    finalState = execState genBlocks initialState
    strategy = let maxReg = nextReg finalState - 1
                in naiveStrategy maxReg
    
    flattenBlock bb = IntMap.findWithDefault [] (bid bb) (codeBlocks finalState)

freshReg :: CodeGen Opnd
freshReg = do
  n <- gets nextReg
  modify $ \s -> s {nextReg = n + 1}
  pure (VirtReg n)
  
assignReg :: NodeId -> Opnd -> CodeGen ()
assignReg nid r = modify $ \s -> s { regMap = Map.insert nid r (regMap s)}
  
lookupReg :: NodeId -> CodeGen Opnd
lookupReg n = do
  m <- gets regMap
  case Map.lookup n m of
    Just r -> pure r
    Nothing -> error $ "Can't find register for " ++ (show n)
    
emit :: BlockId -> Instr -> CodeGen()
emit b instr = modify $ \s -> s { codeBlocks = IntMap.insertWith (flip (++)) b [instr] (codeBlocks s) }

blockLabel :: BlockId -> String
blockLabel b = if b == 0
                 then "_main"
                 else "b" ++ show b

genBlock :: BasicBlock -> CodeGen ()
genBlock bb = do
  emit (bid bb) Nop
  let lbl = blockLabel (bid bb)
  emit (bid bb) (Label lbl)
  modify $ \s -> s { currentBlock = bid bb }
  mapM_ genNode (blockNodes bb)
  
genTerminator :: BasicBlock -> CodeGen ()
genTerminator bb = genNode (blockTerminator bb)
  
genNode :: Node -> CodeGen ()
genNode Node { nid = thisId, nType = nt, block = bid } = case nt of
  ConstNode (IntVal x) -> do
    dst <- freshReg
    emit' $ Mov dst (Imm (show x))
    assignReg thisId dst
    
  ConstNode (BoolVal b) -> do
    dst <- freshReg
    let imm = if b then Imm "1" else Imm "0"
    emit' $ Mov dst imm
    assignReg thisId dst
    
  BinOpNode { binOp = op, left = l, right = r } -> do
    lhs <- lookupReg l
    rhs <- lookupReg r
    dst <- freshReg
    case op of
      IR.Add -> emit' (Mov dst lhs) >> emit' (Add dst rhs)
      IR.Sub -> emit' (Mov dst lhs) >> emit' (Sub dst rhs)
      IR.Mul -> emit' (Mov dst lhs) >> emit' (Imul dst rhs)
      IR.Div -> do
        emit' $ Mov rax32 lhs
        emit' $ Cdq
        emit' $ Idiv rhs
        emit' $ Mov dst rax32
      IR.Mod -> do
        emit' $ Mov rax32 lhs
        emit' $ Cdq
        emit' $ Idiv rhs
        emit' $ Mov dst rdx32
        
      IR.Lt -> emitComp bid Setl lhs rhs dst
      IR.Leq -> emitComp bid Setle lhs rhs dst
      IR.Gt -> emitComp bid Setg lhs rhs dst
      IR.Geq -> emitComp bid Setge lhs rhs dst
      IR.Eq -> emitComp bid Sete lhs rhs dst
      IR.Neq -> emitComp bid Setne lhs rhs dst
      
      IR.Shl -> do
        emit' $ Mov dst lhs
        emit' $ Mov rcx32 rhs
        emit' $ Shl rcx8 dst
      IR.Shr -> do
        emit' $ Mov dst lhs
        emit' $ Mov rcx32 rhs
        emit' $ Mov rcx8 dst
        
      IR.BitAnd -> emit' (Mov dst lhs) >> emit' (And dst rhs)
      IR.BitOr -> emit' (Mov dst lhs) >> emit' (Or dst rhs)
      IR.BitXor -> emit' (Mov dst lhs) >> emit' (Xor dst rhs)
      
      _ -> error ("Can't generate Code for binary operator: " ++ show op)
        
    assignReg thisId dst
    
  UnOpNode { unOp = op, expr = e } -> do
    rhs <- lookupReg e
    dst <- freshReg
    case op of
      IR.Neg -> emit' (Mov dst rhs) >> emit' (Neg dst)
      IR.BitNot -> emit' (Mov dst rhs) >> emit' (Not dst)
      IR.Not -> do
        emit' $ Cmp rhs (Imm "0")
        emit' $ Sete rcx8
        emit' $ Movzx rcx32 rcx8
        emit' $ Mov dst rcx32
    assignReg thisId dst
    
  Jump -> do
    ir <- gets irGraph
    succs <- gets (successors . irGraph)
    let target = blockLabel . block . (getNode ir) . head . IntSet.toList $ succs IntMap.! thisId
    emit' $ Jmp target
    
  Cond cond -> do
    ir <- gets irGraph
    succs <- gets (successors . irGraph)
    let [trueProj, falseProj] = IntSet.toList $ succs IntMap.! thisId
        trueTarget = blockLabel . block . (getNode ir) . head . IntSet.toList $ succs IntMap.! trueProj
        falseTarget = blockLabel . block . (getNode ir) . head . IntSet.toList $ succs IntMap.! falseProj
    
    c <- lookupReg cond
    r <- freshReg
    emit' $ Mov r c
    emit' $ Cmp r (Imm "1")
    emit' $ Je trueTarget
    emit' $ Jmp falseTarget
    
  Return { expr = e } -> do
    r <- lookupReg e
    emit' $ Mov rax32 r
    endBlk <- gets (endBlock . irGraph)
    emit' $ Jmp (blockLabel endBlk)
    
  Phi { preds = ps, isSE = se } -> do
    if se
      then pure ()
      else do
        ir <- gets irGraph
        blk <- gets currentBlock
        r <- freshReg
        assignReg thisId r
        mapM_ (emitPhiMove r) ps
  
  Exit -> emit' Ret
 
  _ -> pure ()
  where
    emit' = emit bid
    
    
 
emitPhiMove :: Opnd -> NodeId -> CodeGen ()
emitPhiMove r n = do
  ir <- gets irGraph
  let blk = block . getNode ir $ n
  e <- lookupReg n
  emit blk $ Mov r e
  
emitComp :: BlockId -> (Opnd -> Instr) -> Opnd -> Opnd -> Opnd -> CodeGen ()
emitComp bid setcc lhs rhs dst = do
  emit' $ Cmp rhs lhs
  emit' $ setcc rcx8
  emit' $ Movzx rcx32 rcx8
  emit' $ Mov dst rcx32
  where
    emit' = emit bid
  
  