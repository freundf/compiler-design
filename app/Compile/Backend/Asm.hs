module Compile.Backend.Asm
  ( codeGen
  ) where
  
import           Compile.Backend.X86.X86
import           Compile.Backend.X86.Register
import           Compile.Backend.X86.Instruction
import           Compile.IR.IRGraph hiding (BinOp(..), UnOp(..))
import qualified Compile.IR.IRGraph as IR
import           Compile.IR.ControlFlow

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
  , cfg :: ControlFlowGraph
  , nextReg :: Integer
  , codeBlocks :: IntMap [Instr]
  }
  
codeGen :: ControlFlowGraph -> X86
codeGen cfg = (X86 defaultDirectives) . concatMap flattenBlock $ (order cfg)
  where
    initialState = CodeGenState Map.empty cfg 0 IntMap.empty
    genBlocks = do
      mapM_ genBlock (order cfg)
      mapM_ genBlockTerminator (order cfg)
    finalState = execState genBlocks initialState
    
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
    Nothing -> error "unreachable, error in semantic analysis"
    
emit :: BlockId -> Instr -> CodeGen()
emit b instr = modify $ \s -> s { codeBlocks = IntMap.insertWith (flip (++)) b [instr] (codeBlocks s) }

blockLabel :: BlockId -> String
blockLabel b = "b" ++ show b

genBlock :: BasicBlock -> CodeGen ()
genBlock bb = do
  let lbl = blockLabel (bid bb)
  emit (bid bb) (Label lbl)
  mapM_ genNode (bNodes bb)
  
genBlockTerminator :: BasicBlock -> CodeGen ()
genBlockTerminator = genTerminator . terminator

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
        emit' $ Movzbl rcx32 rcx8
        emit' $ Mov dst rcx32
    assignReg thisId dst
    
  Phi { preds = ps } -> do
    dst <- freshReg
    let genPhiMove p = do
          bid <- getBlock p
          src <- lookupReg p
          emit bid (Mov dst src)
      
    mapM_ genPhiMove ps
    
    
  _ -> pure ()
  where
    emit' = emit bid
  
  
emitComp :: BlockId -> (Opnd -> Instr) -> Opnd -> Opnd -> Opnd -> CodeGen ()
emitComp bid setcc lhs rhs dst = do
  emit' $ Cmp rhs lhs
  emit' $ setcc rcx8
  emit' $ Movzbl rcx32 rcx8
  emit' $ Mov dst rcx32
  where
    emit' = emit bid
  
  
getBlock :: NodeId -> CodeGen (BlockId)
getBlock n = do
  graph <- gets cfg
  let node = (cfgNodes graph) IntMap.! n
  pure (block node)
  
genTerminator :: Node -> CodeGen ()
genTerminator Node { nType = nt, block = bid }= case nt of
  Branch { cond = c, trueBlk = tBid, falseBlk = fBid } -> do
    condOp <- lookupReg c
    emit bid $ Cmp condOp (Imm "0")
    let trueLbl = blockLabel tBid
        falseLbl = blockLabel fBid
    emit bid $ Jne trueLbl
    emit bid $ Jmp falseLbl
    
  Return { expr = e } -> do
    r <- lookupReg e
    emit bid $ Mov rax32 r
    emit bid $ Ret
    
  _ -> error "emitTerminator: unexpected node type"
  
  