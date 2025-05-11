module Compile.X86 where

data X86 = X86 [Instr]

data Instr
  = Prologue -- hopefully only temporary solution
  | Mov Opnd Opnd
  | Add Opnd Opnd
  | Sub Opnd Opnd
  | Imul Opnd Opnd
  | Idiv Opnd
  | Neg Opnd
  | Cqo
  | Ret
  | Push Opnd
  | Pop Opnd
  
data Opnd
  = VirtReg Integer
  | Reg Reg
  | Imm Integer
  | Mem Reg Integer
  deriving (Eq, Ord)
  
data Reg = RAX | RBX | RCX | RDX | RSP | RBP | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  deriving (Eq, Ord)

tempReg :: [Reg]
tempReg = [R8, R9, R10, R11, R12, R13, R14, R15]

allocStack :: Integer -> [Instr]
allocStack size =
  [ Push (Reg RBP)
  , Mov (Reg RBP) (Reg RSP)
  , Sub (Reg RSP) (Imm size)
  ]

freeStack :: [Instr]
freeStack =
  [ Mov (Reg RSP) (Reg RBP)
  , Pop (Reg RBP)
  ]

instance Show X86 where
  show (X86 instr) = unlines . map (show) $ instr
  
instance Show Instr where
  show (Mov o1 o2)  = "mov " ++ show o1 ++ ", " ++ show o2
  show (Add o1 o2)  = "add " ++ show o1 ++ ", " ++ show o2
  show (Sub o1 o2)  = "sub " ++ show o1 ++ ", " ++ show o2
  show (Imul o1 o2) = "imul " ++ show o1 ++ ", " ++ show o2
  show (Idiv o)     = "idiv " ++ show o
  show (Neg o)      = "neg " ++ show o
  show (Push o)     = "push " ++ show o
  show (Pop o)     = "pop " ++ show o
  show (Cqo)        = "cqo"
  show (Ret)        = "ret"
  show (Prologue) = unlines [
    ".intel_syntax noprefix",
    ".global main",
    ".global _main",
    ".text",
    "",
    "main:",
    "call _main",
    "movq rdi, rax",
    "movq rax, 0x3C",
    "syscall",
    "",
    "_main:"
    ]
  
instance Show Opnd where
  show (VirtReg i) = "v" ++ show i
  show (Reg r) = show r
  show (Imm i) = show i
  show (Mem r i) = if i > 0
                    then "[" ++ show r ++ " + " ++ show i ++ "]"
                    else "[" ++ show r ++ " - " ++ show (abs i) ++ "]"
  
instance Show Reg where
  show RAX = "rax"
  show RBX = "rbx"
  show RCX = "rcx"
  show RDX = "rdx"
  show RSP = "rsp"
  show RBP = "rbp"
  show RSI = "rsi"
  show RDI = "rdi"
  show R8  = "r8"
  show R9  = "r9"
  show R10 = "r10"
  show R11 = "r11"
  show R12 = "r12"
  show R13 = "r14"
  show R14 = "r14"
  show R15 = "r15"
