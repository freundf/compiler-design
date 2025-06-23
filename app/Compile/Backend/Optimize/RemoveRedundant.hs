module Compile.Backend.Optimize.RemoveRedundant
  ( removeRedundant, removeDuplicate
  ) where

import Compile.Backend.Optimize.PeepholeOptimizer
import Compile.Backend.X86.Instruction

removeRedundant :: Optimizer
removeRedundant = Optimizer 1 removeRedundantMov

removeRedundantMov :: [Instr] -> [Instr]
removeRedundantMov [Mov dst src] | dst == src = []
removeRedundantMov xs = xs

removeDuplicate :: Optimizer
removeDuplicate = Optimizer 2 removeDuplicateMov

removeDuplicateMov :: [Instr] -> [Instr]
removeDuplicateMov [Mov dst src, Mov dst' src'] | dst == dst' && src == src' = [Mov dst src]
removeDuplicateMov xs = xs
