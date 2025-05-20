module Compile.X86 where

type X86 = [Instr]

data Instr
  = Prologue -- hopefully only temporary solution
  | Mov Opnd Opnd
  | Add Opnd Opnd
  | Sub Opnd Opnd
  | Imul Opnd Opnd
  | Idiv Opnd
  | Neg Opnd
  | Cdq
  | Ret
  | Push Opnd
  | Pop Opnd

data Opnd
  = VirtReg Integer
  | Reg Reg
  | Imm String
  | Mem Reg Integer
  deriving (Eq, Ord)

data Reg = EAX | EBX | ECX | EDX | RSP | RBP | ESI | EDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  deriving (Eq, Ord)

registers :: [Opnd]
registers = [Reg R8, Reg R9, Reg R10, Reg R11, Reg R12, Reg R13, Reg R14, Reg R15] ++ [Mem RBP (-8 * i) | i <- [1..]]

allocStack :: Integer -> [Instr]
allocStack size
  | size == 0 = []
  | otherwise = [ Push (Reg RBP)
                , Mov (Reg RBP) (Reg RSP)
                , Sub (Reg RSP) (Imm (show size))
                ]

freeStack :: Integer -> [Instr]
freeStack size
  | size == 0 = []
  | otherwise = [ Mov (Reg RSP) (Reg RBP)
                , Pop (Reg RBP)
                ]

printX86 :: X86 -> String
printX86 = unlines . map show

instance Show Instr where
  show (Mov o1 o2)  = "mov " ++ show o1 ++ ", " ++ show o2
  show (Add o1 o2)  = "add " ++ show o1 ++ ", " ++ show o2
  show (Sub o1 o2)  = "sub " ++ show o1 ++ ", " ++ show o2
  show (Imul o1 o2) = "imul " ++ show o1 ++ ", " ++ show o2
  show (Idiv o)     = "idiv " ++ show o
  show (Neg o)      = "neg " ++ show o
  show (Push o)     = "push " ++ show o
  show (Pop o)     = "pop " ++ show o
  show Cdq        = "cdq"
  show Ret        = "ret"
  show Prologue = unlines [
    ".intel_syntax noprefix",
    ".global main",
    ".global _main",
    ".text",
    "",
    "main:",
    "call _main",
    "mov edi, eax",
    "mov eax, 0x3C",
    "syscall",
    "",
    "_main:"
    ]

instance Show Opnd where
  show (VirtReg i) = "v" ++ show i
  show (Reg r) = show r
  show (Imm n) = n
  show (Mem r i) = if i > 0
                    then "[" ++ show r ++ " + " ++ show i ++ "]"
                    else "[" ++ show r ++ " - " ++ show (abs i) ++ "]"

instance Show Reg where
  show EAX = "eax"
  show EBX = "ebx"
  show ECX = "ecx"
  show EDX = "edx"
  show RSP = "rsp"
  show RBP = "rbp"
  show ESI = "esi"
  show EDI = "edi"
  show R8  = "r8d"
  show R9  = "r9d"
  show R10 = "r10d"
  show R11 = "r11d"
  show R12 = "r12d"
  show R13 = "r13d"
  show R14 = "r14d"
  show R15 = "r15d"
