module Compile.RegAlloc
  ( regAlloc, naiveStrategy, coloringStrategy
  ) where

import           Compile.X86
import           Compile.Liveness
import           Compile.GraphColoring

import           Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as Map


type Allocator a = State AllocState a

type RegAlloc = Map Opnd Opnd

data AllocState = AllocState
  { regMap :: Map Opnd Opnd
  , code :: [Instr]
  }

initialState :: RegAlloc -> AllocState
initialState regs = AllocState
  { regMap = regs
  , code = []
  }

emit :: Instr -> Allocator ()
emit instr = modify $ \s -> s { code = code s ++ [instr] }

processInstr :: Instr -> Allocator ()
processInstr (Mov o1 o2) = do
  regs <- gets regMap
  let r1 = getReg o1 regs
  let r2 = getReg o2 regs
  case (r1, r2) of
    (Mem _ _, Mem _ _) -> do
      emit $ Mov (Reg ECX) r2
      emit $ Mov r1 (Reg ECX)
    (Mem _ _, Imm _) -> do
      emit $ Mov (Reg ECX) r2
      emit $ Mov r1 (Reg ECX)
    _ -> emit $ Mov r1 r2
processInstr (Add o1 o2) = processBinOp Add o1 o2
processInstr (Sub o1 o2) = processBinOp Sub o1 o2
processInstr (Imul o1 o2) = processBinOp Imul o1 o2
processInstr (Idiv o) = do
    regs <- gets regMap
    let r = getReg o regs
    case r of
      (Mem _ _) -> do
        emit $ Mov (Reg ECX) r
        emit $ Idiv (Reg ECX)
      _ -> emit $ Idiv r
processInstr (Neg o) = do
    regs <- gets regMap
    let r = getReg o regs
    case r of
      (Mem _ _) -> do
        emit $ Mov (Reg ECX) r
        emit $ Neg (Reg ECX)
        emit $ Mov r (Reg ECX)
      _ -> emit (Neg r)
processInstr instr = emit instr

processBinOp :: (Opnd -> Opnd -> Instr) -> Opnd -> Opnd -> Allocator ()
processBinOp instr o1 o2 = do
    regs <- gets regMap
    let r1 = getReg o1 regs
    let r2 = getReg o2 regs
    case (r1, r2) of
      (Mem _ _, _) -> do
        emit $ Mov (Reg ECX) r1
        emit $ instr (Reg ECX) r2
        emit $ Mov r1 (Reg ECX)
      _ -> emit $ instr r1 r2


getReg :: (Ord a) => a -> Map a a -> a
getReg reg = Map.findWithDefault reg reg


regAlloc :: X86 -> RegAlloc -> X86
regAlloc instrs strategy = reserveStack stackUsed (code finalState)
  where
    finalState = execState (mapM_ processInstr instrs) (initialState strategy)
    stackUsed = maximum . map (abs . getOffset . snd) . Map.toList $ strategy
    getOffset (Mem _ offset) = offset
    getOffset _ = 0
    
coloringStrategy :: X86 -> RegAlloc
coloringStrategy instrs = colorGraph registers (livenessGraph (liveness instrs))

reserveStack :: Integer -> X86 -> X86
reserveStack s instr = allocStack s ++ reserveStack' s instr
  where
    reserveStack' _ [] = []
    reserveStack' s' (i:is) = case i of
      Ret -> freeStack s' ++ [Ret] ++ reserveStack' s' is
      _     -> i : reserveStack' s' is

naiveStrategy :: Integer -> RegAlloc
naiveStrategy maxOffset = Map.fromList $ [
        (VirtReg 0, Reg R8),
        (VirtReg 1, Reg R9),
        (VirtReg 2, Reg R10),
        (VirtReg 3, Reg R11),
        (VirtReg 4, Reg R12),
        (VirtReg 5, Reg R13),
        (VirtReg 6, Reg R14),
        (VirtReg 7, Reg R15)
    ] ++ [(VirtReg (i + 7), Mem RBP (- (8 * i))) | i <- [1 .. (maxOffset - 7)]]
