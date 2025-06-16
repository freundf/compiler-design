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
import           Control.Monad (forM_)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.IntSet  (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (findIndex)
import Data.Maybe (fromJust)

import Control.Monad.IO.Class
import Debug.Trace (traceShow)

type CodeGen a = State CodeGenState a

data CodeGenState = CodeGenState
  { regMap :: Map NodeId Opnd
  , order :: Schedule
  , irGraph :: IRGraph
  , nextReg :: Int
  , currentBlock :: Int
  , codeBlocks :: IntMap [Instr]
  , phiMoves :: IntMap [Instr]
  }
  
codeGen :: IRGraph -> X86
codeGen ir = regAlloc ((X86 defaultDirectives) . (prologue ++) . concatMap flattenBlock $ schedule ir) strategy
  where
    order = schedule ir
    regs = let x = preAlloc ir in traceShow x x
    initialState = CodeGenState regs order ir (Map.size regs) 0 IntMap.empty IntMap.empty
    genBlocks = do
      mapM_ genBlock order
      mapM_ genTerminator order
    finalState = execState genBlocks initialState
    strategy = let maxReg = nextReg finalState - 1
                in naiveStrategy maxReg
    
    flattenBlock bb = IntMap.findWithDefault [] (bid bb) (codeBlocks finalState)

preAlloc :: IRGraph -> Map NodeId Opnd
preAlloc graph = Map.fromList $ zip ns (map VirtReg [0..])
  where
    ns = map nid . filter needsRegister . IntMap.elems . nodes $ graph
    needsRegister n = case nType n of
      BinOpNode {} -> True
      UnOpNode {} -> True
      Phi {} -> True
      Proj {} -> True
      Cond {} -> True
      _ -> False
      
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
genTerminator bb = do
  let b = bid bb
  phis <- gets (IntMap.findWithDefault [] b . phiMoves)
  mapM_ (emit b) phis
  genNode (blockTerminator bb)
  
genNode :: Node -> CodeGen ()
genNode Node { nid = thisId, nType = nt, block = bid } = case nt of
  ConstNode (IntVal x) -> do
    --dst <- lookupReg thisId
    --emit' $ Mov dst (Imm (show x))
    assignReg thisId (Imm (show x))
    
  ConstNode (BoolVal b) -> do
    --dst <- lookupReg thisId
    let imm = if b then Imm "1" else Imm "0"
    --emit' $ Mov dst imm
    assignReg thisId imm
    
  BinOpNode { binOp = op, left = l, right = r } -> do
    lhs <- lookupReg l
    rhs <- lookupReg r
    dst <- lookupReg thisId
    case op of
      IR.Add -> emit' (Mov dst lhs) >> emit' (Add dst rhs)
      IR.Sub -> emit' (Mov dst lhs) >> emit' (Sub dst rhs)
      IR.Mul -> emit' (Mov dst lhs) >> emit' (Imul dst rhs)
      IR.Div -> do
        emit' $ Mov rax32 lhs
        emit' $ Cdq
        case rhs of
          Imm {} -> emit' (Mov transferReg rhs) >> emit' (Idiv transferReg)
          _ -> emit' $ Idiv rhs
        emit' $ Mov dst rax32
      IR.Mod -> do
        emit' $ Mov rax32 lhs
        emit' $ Cdq
        case rhs of
          Imm {} -> emit' (Mov transferReg rhs) >> emit' (Idiv transferReg)
          _ -> emit' $ Idiv rhs
        emit' $ Mov dst rdx32
        
      IR.Lt -> emitComp thisId bid Setl lhs rhs dst
      IR.Leq -> emitComp thisId bid Setle lhs rhs dst
      IR.Gt -> emitComp thisId bid Setg lhs rhs dst
      IR.Geq -> emitComp thisId bid Setge lhs rhs dst
      IR.Eq -> emitComp thisId bid Sete lhs rhs dst
      IR.Neq -> emitComp thisId bid Setne lhs rhs dst
      
      IR.Shl -> do
        emit' $ Mov dst lhs
        emit' $ Mov rcx32 rhs
        emit' $ Sall dst rcx8
      IR.Shr -> do
        emit' $ Mov dst lhs
        emit' $ Mov rcx32 rhs
        emit' $ Sarl dst rcx8
        
      IR.BitAnd -> emit' (Mov dst lhs) >> emit' (And dst rhs)
      IR.BitOr -> emit' (Mov dst lhs) >> emit' (Or dst rhs)
      IR.BitXor -> emit' (Mov dst lhs) >> emit' (Xor dst rhs)
      
      _ -> error ("Can't generate Code for binary operator: " ++ show op)
        
    
  UnOpNode { unOp = op, expr = e } -> do
    rhs <- lookupReg e
    dst <- lookupReg thisId
    case op of
      IR.Neg -> emit' (Mov dst rhs) >> emit' (Neg dst)
      IR.BitNot -> emit' (Mov dst rhs) >> emit' (Not dst)
      IR.Not -> do
        emit' $ Cmp rhs (Imm "0")
        emit' $ Sete rcx8
        emit' $ Movzx rcx32 rcx8
        emit' $ Mov dst rcx32
    
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
    r <- lookupReg thisId
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
        r <- lookupReg thisId
        forM_ ps $ \p -> do
          src <- case nType (getNode ir p) of
                      ConstNode { value = IntVal i } -> pure (Imm (show i))
                      ConstNode { value = BoolVal b } -> if b then pure (Imm "1") else pure (Imm "0")
                      _ -> lookupReg p
          let predBlk = block (getNode ir p)
          modify $ \s -> s { phiMoves = IntMap.insertWith (++) predBlk [Mov r src] (phiMoves s) }
  
  Exit -> emit' Ret
 
  Proj { expr = e, projInfo = info } -> do
    case info of
      Result -> do
        r1 <- lookupReg thisId
        r2 <- lookupReg e
        emit' $ Mov r1 r2
      _ -> pure ()
 
  _ -> pure ()
  where
    emit' = emit bid
    
 
emitPhiMove :: Opnd -> NodeId -> CodeGen ()
emitPhiMove r n = do
  ir <- gets irGraph
  let blk = block . getNode ir $ n
  e <- lookupReg n
  emit blk $ Mov r e
  
emitComp :: NodeId -> BlockId -> (Opnd -> Instr) -> Opnd -> Opnd -> Opnd -> CodeGen ()
emitComp thisId bid setcc lhs rhs dst = do
  r <- lookupReg thisId
  emit' $ Mov r lhs
  emit' $ Cmp r rhs
  emit' $ setcc rcx8
  emit' $ Movzx rcx32 rcx8
  emit' $ Mov dst rcx32
  where
    emit' = emit bid
  
  