module Compile.Backend.Optimize.PeepholeOptimizer where

import Compile.Backend.X86.X86
import Compile.Backend.X86.Instruction
import Compile.Backend.X86.Register

import Data.List (foldl')

data Optimizer = Optimizer
  { windowSize :: Int
  , optimize :: [Instr] -> [Instr]
  }

peepholeOptimize :: [Optimizer] -> [Instr] -> [Instr]
peepholeOptimize opts = fixpoint (applyOptimizers opts)

applyOptimizers :: [Optimizer] -> [Instr] -> [Instr]
applyOptimizers opts instrs = foldl' (\cur opt -> optimizeWindow (windowSize opt) (optimize opt) cur) instrs opts

optimizeWindow :: Int -> ([Instr] -> [Instr]) -> [Instr] -> [Instr]
optimizeWindow n f instrs = optimize' instrs []
  where
    optimize' xs acc
      | not (hasAtLeast n xs) = reverse acc ++ xs
      | otherwise =
        let (win, rest) = splitAt n xs
            optimized = f win
        in if optimized == win
            then
              optimize' (tail xs) (head xs : acc)
            else
              optimize' rest (reverse optimized ++ acc)

hasAtLeast :: Int -> [a] -> Bool
hasAtLeast 0 _ =  True
hasAtLeast 1 [] = False
hasAtLeast k (_:xs) = hasAtLeast (k - 1) xs

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x =
  let x' = f x
  in if x == x' then x else fixpoint f x'

testX86 :: [Instr]
testX86 = [Mov rax64 rbx64, Mov rbx64 rax64, Mov rax64 rbx64, Mov rcx64 rcx64, Nop]