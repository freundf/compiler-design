module Compile.IR.RegAlloc
  ( regAlloc, naiveStrategy
  ) where

import           Compile.Backend.X86.X86
import           Compile.Backend.X86.Register
import           Compile.Backend.X86.Instruction

import           Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as Map


type Allocator a = State AllocState a

type RegAlloc = Map Opnd Opnd

data AllocState = AllocState
  { regMap :: Map Opnd Opnd
  , x86Code :: [Instr]
  }

initialState :: RegAlloc -> AllocState
initialState regs = AllocState
  { regMap = regs
  , x86Code = []
  }
  
transferReg :: Opnd
transferReg = Reg (Register R8 Size32)

emit :: Instr -> Allocator ()
emit instr = modify $ \s -> s { x86Code = x86Code s ++ [instr] }

processInstr :: Instr -> Allocator ()
processInstr instr = case instr of
  Mov o1 o2 -> processMov o1 o2
  Movzx o1 o2 -> processBinOp Movzx o1 o2
  
  Add o1 o2 -> processBinOp Add o1 o2
  Sub o1 o2 -> processBinOp Sub o1 o2
  Imul o1 o2 -> processBinOp Imul o1 o2
  Idiv o -> processIdiv o
  Neg o -> processUnary Neg o
  Cdq -> emit instr
  
  And o1 o2 -> processBinOp And o1 o2
  Or o1 o2 -> processBinOp Or o1 o2
  Xor o1 o2 -> processBinOp Xor o1 o2
  Not o -> processUnary Not o
  
  Sall o1 o2 -> processBinOp Sall o1 o2
  Sarl o1 o2 -> processBinOp Sarl o1 o2
  
  Cmp o1 o2 -> processBinOp Cmp o1 o2
  Setl o -> processUnary Setl o
  Setle o -> processUnary Setle o
  Setg o -> processUnary Setg o
  Setge o -> processUnary Setge o
  Sete o -> processUnary Sete o
  Setne o -> processUnary Setne o
  
  Push o -> processUnary Push o
  Pop o -> processUnary Pop o
  
  _ -> emit instr
  
processBinOp :: (Opnd -> Opnd -> Instr) -> Opnd -> Opnd -> Allocator ()
processBinOp instr o1 o2 = do
    regs <- gets regMap
    let r1 = getReg o1 regs
    let r2 = getReg o2 regs
    case (r1, r2) of
      (Mem _ _, _) -> do
        emit $ Mov transferReg r1
        emit $ instr transferReg r2
        emit $ Mov r1 transferReg
      _ -> emit $ instr r1 r2

processUnary :: (Opnd -> Instr) -> Opnd -> Allocator ()
processUnary instr o = do
  regs <- gets regMap
  let r = getReg o regs
  case r of
    Mem _ _ -> do
      emit $ Mov transferReg r
      emit $ instr transferReg
      emit $ Mov r transferReg
    _ -> emit $ instr r

processMov :: Opnd -> Opnd -> Allocator ()
processMov o1 o2 = do
  regs <- gets regMap
  let r1 = getReg o1 regs
  let r2 = getReg o2 regs
  case (r1, r2) of
    (Mem _ _, Mem _ _) -> do
      emit $ Mov transferReg r2
      emit $ Mov r1 transferReg
    (Mem _ _, Imm _) -> do
      emit $ Mov transferReg r2
      emit $ Mov r1 transferReg
    _ -> emit $ Mov r1 r2
      
processIdiv :: Opnd -> Allocator ()
processIdiv o = do
  regs <- gets regMap
  let r = getReg o regs
  case r of
    (Mem _ _) -> do
      emit $ Mov transferReg r
      emit $ Idiv transferReg
    _ -> emit $ Idiv r
    
getReg :: (Ord a) => a -> Map a a -> a
getReg reg = Map.findWithDefault reg reg


regAlloc :: X86 -> RegAlloc -> X86
regAlloc (X86 d instr) strategy = (X86 d) $ reserveStack stackUsed (x86Code finalState)
  where
    finalState = execState (mapM_ processInstr instr) (initialState strategy)
    stackUsed = maximum . map (abs . getOffset . snd) . Map.toList $ strategy
    getOffset (Mem _ offset) = offset
    getOffset _ = 0
    
--coloringStrategy :: X86 -> RegAlloc
--coloringStrategy instrs = colorGraph registers (livenessGraph (liveness instrs))

reserveStack :: Int -> [Instr] -> [Instr]
reserveStack s instr = (head instr) : allocStack s ++ reserveStack' s (tail instr)
  where
    reserveStack' _ [] = []
    reserveStack' s' (i:is) = case i of
      Ret -> freeStack s' ++ [Ret] ++ reserveStack' s' is
      _     -> i : reserveStack' s' is

naiveStrategy :: Int -> RegAlloc
naiveStrategy maxOffset = Map.fromList $ [
        (VirtReg 0, Reg (Register R9 Size32)),
        (VirtReg 1, Reg (Register R10 Size32)),
        (VirtReg 2, Reg (Register R11 Size32)),
        (VirtReg 3, Reg (Register R12 Size32)),
        (VirtReg 4, Reg (Register R13 Size32)),
        (VirtReg 5, Reg (Register R14 Size32)),
        (VirtReg 6, Reg (Register R15 Size32))
      ] ++ [(VirtReg (i + 6), Mem (Register RBP Size64) (- (8 * i))) | i <- [1 .. (maxOffset - 7)]]
