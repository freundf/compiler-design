module Compile.Backend.X86.Instruction where
  
import Compile.Backend.X86.Register


data Instr
  -- Move
  = Mov Opnd Opnd
  | Movzbl Opnd Opnd
  
  -- Arithmetic
  | Add Opnd Opnd
  | Sub Opnd Opnd
  | Imul Opnd Opnd
  | Idiv Opnd
  | Neg Opnd
  | Cdq
  
  -- Bitwise
  | And    Opnd Opnd      -- and  src, dst    (bitwise AND)
  | Or     Opnd Opnd      -- or   src, dst    (bitwise OR)
  | Xor    Opnd Opnd      -- xor  src, dst    (bitwise XOR)
  | Not    Opnd
  -- Shift
  | Shl    Opnd Opnd      -- shl  amt, dst    (logical left shift; amt can be “1” or “cl”)
  | Shr    Opnd Opnd      -- shr  amt, dst    (logical right shift)
  
  -- Setcc / Comparison
  | Cmp    Opnd Opnd      -- cmp  lhs, rhs    (sets flags lhs–rhs)
  | Setl Opnd
  | Setle Opnd
  | Setg Opnd
  | Setge Opnd
  | Sete Opnd
  | Setne Opnd
  
  -- Control
  | Jmp    String         -- jmp  <label>
  | Jne    String         -- jne  <label>
  | Je     String         -- je   <label>
  | Jl     String         -- jl   <label>
  | Jle    String         -- jle  <label>
  | Jg     String         -- jg   <label>
  | Jge    String         -- jge  <label>
  | Call String
  | Syscall
  | Ret
  
  -- Stack
  | Push Opnd
  | Pop Opnd
  
  -- Other
  | Label String
  | Nop
  deriving (Eq)

data Opnd
  = VirtReg Integer
  | Reg Register
  | Imm String
  | Mem Register Integer
  deriving (Eq, Ord)
  
  

instance Show Instr where
  -- Move
  show (Mov o1 o2)  = "mov " ++ show o1 ++ ", " ++ show o2
  show (Movzbl o1 o2) = "movzbl " ++ show o1 ++ ", " ++ show o2
  
  -- Arithmetic
  show (Add o1 o2)  = "add " ++ show o1 ++ ", " ++ show o2
  show (Sub o1 o2)  = "sub " ++ show o1 ++ ", " ++ show o2
  show (Imul o1 o2) = "imul " ++ show o1 ++ ", " ++ show o2
  show (Idiv o)     = "idiv " ++ show o
  show (Neg o)      = "neg " ++ show o
  show Cdq          = "cdq"
  
  -- Logic
  show (And o1 o2)    = "and " ++ show o1 ++ ", " ++ show o2
  show (Or  o1 o2)    = "or "  ++ show o1 ++ ", " ++ show o2
  show (Xor o1 o2)    = "xor " ++ show o1 ++ ", " ++ show o2
  
  -- Shift
  show (Shl amt dst)  = "shl " ++ show amt ++ ", " ++ show dst
  show (Shr amt dst)  = "shr " ++ show amt ++ ", " ++ show dst
  
  -- Setcc / Comparison
  show (Cmp o1 o2)    = "cmp "  ++ show o1 ++ ", " ++ show o2
  show (Setl o)       = "setl " ++ show o
  show (Setle o)      = "setle " ++ show o
  show (Setg o)       = "setg " ++ show o
  show (Setge o)      = "setge " ++ show o
  show (Sete o)       = "sete " ++ show o
  show (Setne o)      = "setne " ++ show o
  
  -- Control
  show (Jmp lbl)      = "jmp "  ++ lbl
  show (Jne lbl)      = "jne "  ++ lbl
  show (Je lbl)       = "je "   ++ lbl
  show (Jl lbl)       = "jl "   ++ lbl
  show (Jle lbl)      = "jle "  ++ lbl
  show (Jg lbl)       = "jg "   ++ lbl
  show (Jge lbl)      = "jge "  ++ lbl
  show Ret            = "ret"
  show (Call s)       = "    call " ++ s
  show Syscall        = "    syscall"
  
  -- Stack
  show (Push o)     = "push " ++ show o
  show (Pop o)     = "pop " ++ show o
 
  -- Other
  show (Label s)      = s ++ ":"
  show Nop            = "    nop"


instance Show Opnd where
  show (VirtReg i) = "v" ++ show i
  show (Reg r) = show r
  show (Imm n) = n
  show (Mem r i) = if i > 0
                    then "[" ++ show r ++ " + " ++ show i ++ "]"
                    else "[" ++ show r ++ " - " ++ show (abs i) ++ "]"

   
   
rax8  = Reg $ Register RAX Size8
rax16 = Reg $ Register RAX Size16
rax32 = Reg $ Register RAX Size32
rax64 = Reg $ Register RAX Size64

rbx8  = Reg $ Register RBX Size8
rbx16 = Reg $ Register RBX Size16
rbx32 = Reg $ Register RBX Size32
rbx64 = Reg $ Register RBX Size64

rcx8  = Reg $ Register RCX Size8
rcx16 = Reg $ Register RCX Size16
rcx32 = Reg $ Register RCX Size32
rcx64 = Reg $ Register RCX Size64

rdx8  = Reg $ Register RDX Size8
rdx16 = Reg $ Register RDX Size16
rdx32 = Reg $ Register RDX Size32
rdx64 = Reg $ Register RDX Size64

rbp8  = Reg $ Register RBP Size8
rbp16 = Reg $ Register RBP Size16
rbp32 = Reg $ Register RBP Size32
rbp64 = Reg $ Register RBP Size64

rsp8  = Reg $ Register RSP Size8
rsp16 = Reg $ Register RSP Size16
rsp32 = Reg $ Register RSP Size32
rsp64 = Reg $ Register RSP Size64

rdi8  = Reg $ Register RDI Size8
rdi16 = Reg $ Register RDI Size16
rdi32 = Reg $ Register RDI Size32
rdi64 = Reg $ Register RDI Size64