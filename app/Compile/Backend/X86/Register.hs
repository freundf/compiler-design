module Compile.Backend.X86.Register where

data Register = Register
  { name :: RegName
  , size :: RegSize
  } deriving (Eq, Ord)
  
data RegName = RAX | RBX | RCX | RDX | RSP | RBP | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  deriving (Eq, Ord)

data RegSize = Size8 | Size16 | Size32 | Size64
  deriving (Eq, Ord)

instance Show Register where
  show (Register reg sz) = case sz of
    Size8  -> show8 reg
    Size16 -> show16 reg
    Size32 -> show32 reg
    Size64 -> show64 reg
    
    
show8 :: RegName -> String
show8 r = case r of
  RAX -> "al"
  RBX -> "bl"
  RCX -> "cl"
  RDX -> "dl"
  RSI -> "sil"
  RDI -> "dil"
  RSP -> "spl"
  RBP -> "bpl"
  R8  -> "r8b"
  R9  -> "r9b"
  R10 -> "r10b"
  R11 -> "r11b"
  R12 -> "r12b"
  R13 -> "r13b"
  R14 -> "r14b"
  R15 -> "r15b"
  
show16 :: RegName -> String
show16 r = case r of
  RAX -> "ax"
  RBX -> "bx"
  RCX -> "cx"
  RDX -> "dx"
  RSI -> "si"
  RDI -> "di"
  RSP -> "sp"
  RBP -> "bp"
  R8  -> "r8w"
  R9  -> "r9w"
  R10 -> "r10w"
  R11 -> "r11w"
  R12 -> "r12w"
  R13 -> "r13w"
  R14 -> "r14w"
  R15 -> "r15w"

show32 :: RegName -> String
show32 r = case r of
  RAX -> "eax"
  RBX -> "ebx"
  RCX -> "ecx"
  RDX -> "edx"
  RSI -> "esi"
  RDI -> "edi"
  RSP -> "esp"
  RBP -> "ebp"
  R8  -> "r8d"
  R9  -> "r9d"
  R10 -> "r10d"
  R11 -> "r11d"
  R12 -> "r12d"
  R13 -> "r13d"
  R14 -> "r14d"
  R15 -> "r15d"

show64 :: RegName -> String
show64 r = case r of
  RAX -> "rax"
  RBX -> "rbx"
  RCX -> "rcx"
  RDX -> "rdx"
  RSI -> "rsi"
  RDI -> "rdi"
  RSP -> "rsp"
  RBP -> "rbp"
  R8  -> "r8"
  R9  -> "r9"
  R10 -> "r10"
  R11 -> "r11"
  R12 -> "r12"
  R13 -> "r13"
  R14 -> "r14"
  R15 -> "r15"




