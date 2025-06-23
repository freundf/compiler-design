module Compile.IR.RegAlloc
  ( naiveStrategy, transferReg, RegAlloc(..)
  ) where

import           Compile.Backend.X86.X86
import           Compile.Backend.X86.Register
import           Compile.Backend.X86.Instruction
import           Compile.IR.IRGraph

import           Control.Monad.State
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

transferReg :: Opnd
transferReg = rcx32

data RegAlloc = RegAlloc
  { regMap :: IntMap Opnd
  , stackSpace :: Int
  } deriving (Eq, Show)

naiveStrategy :: Int -> RegAlloc
naiveStrategy maxOffset = RegAlloc
  { regMap = IntMap.fromList $ [
                        (0, Reg (Register R9 Size32)),
                        (1, Reg (Register R10 Size32)),
                        (2, Reg (Register R11 Size32)),
                        (3, Reg (Register R12 Size32)),
                        (4, Reg (Register R13 Size32)),
                        (5, Reg (Register R14 Size32)),
                        (6, Reg (Register R15 Size32))
                        ] ++ [(i + 6, Mem (Register RBP Size64) (- (8 * i))) | i <- [1 .. (maxOffset - 6)]]
  , stackSpace = 8 * (maxOffset - 6)
  }