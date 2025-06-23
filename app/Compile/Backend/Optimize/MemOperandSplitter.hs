module Compile.Backend.Optimize.MemOperandSplitter
  ( memOperandSplitter
  ) where

import Compile.Backend.Optimize.PeepholeOptimizer
import Compile.Backend.X86.Instruction

memOperandSplitter :: Opnd -> Optimizer
memOperandSplitter transferReg = Optimizer 1 (splitMemOperands transferReg)

splitMemOperands :: Opnd -> [Instr] -> [Instr]
splitMemOperands transferReg [i] = case i of
  Mov o1 o2 -> if isMem o1 && isMem o2
                  then [Mov transferReg o2, Mov o1 transferReg]
                  else [Mov o1 o2]
  Add o1 o2 -> processInstr Add o1 o2
  Sub o1 o2 -> processInstr Sub o1 o2
  Imul o1 o2 -> if isMem o1 && isImm o2
                  then [Mov transferReg o1, Imul transferReg o2, Mov o1 transferReg]
                  else processInstr Imul o1 o2
  Cmp o1 o2 -> processInstr Cmp o1 o2
  And o1 o2 -> processInstr And o1 o2
  Or o1 o2 -> processInstr Or o1 o2
  Xor o1 o2 -> processInstr Xor o1 o2
  Sar o1 o2 -> processInstr Sar o1 o2
  Sal o1 o2 -> processInstr Sal o1 o2
  _ -> [i]
  where
    processInstr :: (Opnd -> Opnd -> Instr) -> Opnd -> Opnd -> [Instr]
    processInstr instr o1 o2
      | isMem o1 && isMem o2  = [ Mov transferReg o1
                                , instr transferReg o2
                                , Mov o1 transferReg
                                ]
      | otherwise             = [instr o1 o2]

splitMemOperands _ xs = xs

isMem :: Opnd -> Bool
isMem (Mem _ _) = True
isMem _         = False

isImm :: Opnd -> Bool
isImm (Imm _) = True
isImm _       = False