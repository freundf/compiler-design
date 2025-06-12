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

emit :: Instr -> Allocator ()
emit instr = modify $ \s -> s { x86Code = x86Code s ++ [instr] }

processInstr :: Instr -> Allocator ()
processInstr (Mov o1 o2) = do
  regs <- gets regMap
  let r1 = getReg o1 regs
  let r2 = getReg o2 regs
  case (r1, r2) of
    (Mem _ _, Mem _ _) -> do
      emit $ Mov rcx32 r2
      emit $ Mov r1 rcx32
    (Mem _ _, Imm _) -> do
      emit $ Mov rcx32 r2
      emit $ Mov r1 rcx32
    _ -> emit $ Mov r1 r2
processInstr (Add o1 o2) = processBinOp Add o1 o2
processInstr (Sub o1 o2) = processBinOp Sub o1 o2
processInstr (Imul o1 o2) = processBinOp Imul o1 o2
processInstr (Idiv o) = do
    regs <- gets regMap
    let r = getReg o regs
    case r of
      (Mem _ _) -> do
        emit $ Mov rcx32 r
        emit $ Idiv rcx32
      _ -> emit $ Idiv r
processInstr (Neg o) = do
    regs <- gets regMap
    let r = getReg o regs
    case r of
      (Mem _ _) -> do
        emit $ Mov rcx32 r
        emit $ Neg rcx32
        emit $ Mov r rcx32
      _ -> emit (Neg r)
processInstr (Cmp o1 o2) = processBinOp Cmp o1 o2
processInstr instr = emit instr

processBinOp :: (Opnd -> Opnd -> Instr) -> Opnd -> Opnd -> Allocator ()
processBinOp instr o1 o2 = do
    regs <- gets regMap
    let r1 = getReg o1 regs
    let r2 = getReg o2 regs
    case (r1, r2) of
      (Mem _ _, _) -> do
        emit $ Mov rcx32 r1
        emit $ instr rcx32 r2
        emit $ Mov r1 rcx32
      _ -> emit $ instr r1 r2


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

reserveStack :: Integer -> [Instr] -> [Instr]
reserveStack s instr = (head instr) : allocStack s ++ reserveStack' s (tail instr)
  where
    reserveStack' _ [] = []
    reserveStack' s' (i:is) = case i of
      Ret -> freeStack s' ++ [Ret] ++ reserveStack' s' is
      _     -> i : reserveStack' s' is

naiveStrategy :: Integer -> RegAlloc
naiveStrategy maxOffset = Map.fromList $ [
        (VirtReg 0, Reg (Register R8 Size32)),
        (VirtReg 1, Reg (Register R9 Size32)),
        (VirtReg 2, Reg (Register R10 Size32)),
        (VirtReg 3, Reg (Register R11 Size32)),
        (VirtReg 4, Reg (Register R12 Size32)),
        (VirtReg 5, Reg (Register R13 Size32)),
        (VirtReg 6, Reg (Register R14 Size32)),
        (VirtReg 7, Reg (Register R15 Size32))
    ] ++ [(VirtReg (i + 7), Mem (Register RBP Size64) (- (8 * i))) | i <- [1 .. (maxOffset - 7)]]
