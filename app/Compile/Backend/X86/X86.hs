module Compile.Backend.X86.X86 where

import Compile.Backend.X86.Register
import Compile.Backend.X86.Instruction

import Data.List (intersect)

data X86 = X86
  { directives :: Directives
  , code :: [Instr]
  }

data SyntaxType = Intel | ATT

data Directives = Directives
  { syntax :: SyntaxType
  , global :: [String]
  , extern :: [String]
  }

defaultDirectives :: Directives
defaultDirectives = Directives { syntax = Intel, global = ["main", "main_"], extern = ["print", "read", "flush"] }

prologue :: [Instr]
prologue =
  [ Label "main"
  , Call "_main"
  , Mov r12_64 rax64
  , Call "flush"
  , Mov rdi64 r12_64
  , Mov rax64 (Imm "0x3C")
  , Syscall
  ]

functionPrologue :: [Register] -> [Instr]
functionPrologue used = [ Push rbp64, Mov rbp64 rsp64 ] ++ [ Push (Reg x) | x <- intersect calleeSaved used ]

functionEpilogue :: [Register] -> [Instr]
functionEpilogue used = [ Pop (Reg x) | x <- intersect calleeSaved used ] ++ [ Pop rbp64, Ret]

registers :: [Opnd]
registers =
  [ Reg (Register R8 Size32)
  , Reg (Register R9 Size32)
  , Reg (Register R10 Size32)
  , Reg (Register R11 Size32)
  , Reg (Register R12 Size32)
  , Reg (Register R13 Size32)
  , Reg (Register R14 Size32)
  , Reg (Register R15 Size32)
  ]
  ++ [Mem (Register RBP Size32) (-8 * i) | i <- [1..]]

calleeSaved :: [Register]
calleeSaved =
  [ Register RBX Size64
  , Register R12 Size64
  , Register R13 Size64
  , Register R14 Size64
  , Register R15 Size64
  ]

callerSaved :: [Register]
callerSaved =
  [ Register RDI Size64
  , Register RSI Size64
  , Register RCX Size64
  , Register RDX Size64
  , Register R8 Size64
  , Register R9 Size64
  , Register R10 Size64
  , Register R10 Size64
  ]

allocStack :: Int -> [Instr]
allocStack size
  | size == 0 = []
  | otherwise = [ Push rbp64
                , Mov rbp64 rsp64
                , Sub rsp64 (Imm (show size))
                ]

freeStack :: Int -> [Instr]
freeStack size
  | size == 0 = []
  | otherwise = [ Mov rsp64 rbp64
                , Pop rbp64
                ]

printX86 :: X86 -> String
printX86 x86 = show (directives x86) ++ (unlines . map show $ code x86)


instance Show Directives where
  show d = showSyntax ++ showGlobals ++ showExterns ++ ".text \n\n"
    where
      showSyntax = case syntax d of
        Intel -> ".intel_syntax noprefix \n"
        ATT -> ""

      showGlobals = concatMap (\s -> ".global " ++ s ++ "\n") (global d)
      showExterns = concatMap (\s -> ".extern " ++ s ++ "\n") (extern d)