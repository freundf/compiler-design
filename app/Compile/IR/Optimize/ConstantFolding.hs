module Compile.IR.Optimize.ConstantFolding where

import Compile.IR.IRGraph

import Control.Monad.State
import Data.Bits ((.|.), (.&.), complement, xor, shiftL, shiftR)
import Data.Int (Int32)

constantFold :: IRGraph -> Node -> Node
constantFold ir node = do
  case nType node of
    BinOpNode { left = lhs, right = rhs } ->
      case (nType (getNode ir lhs), nType (getNode ir rhs)) of
        (ConstNode lVal, ConstNode rVal) -> foldBinOp node lVal rVal
        _ -> node
        
    UnOpNode { expr = e } ->
      case (nType (getNode ir e)) of
        ConstNode val -> foldUnOp node val
        _ -> node
        
    _ -> node
  

foldUnOp :: Node -> Value -> Node
foldUnOp node val =
  let i = intVal val
      ty = nType node
  
  in case unOp ty of
    Neg -> makeConst node (IntVal (-i))
    Not -> makeConst node (IntVal (1 - i))
    BitNot -> makeConst node (IntVal (complement i))
  
foldBinOp :: Node -> Value -> Value -> Node
foldBinOp node lVal rVal =
  let left = intVal lVal
      right = intVal rVal
      ty = nType node
      
  in case binOp ty of
    Mul -> makeConst node (IntVal (left * right))
    Add -> makeConst node (IntVal (left + right))
    Sub -> makeConst node (IntVal (left - right))
    Div -> if right == 0 || (left == (minBound :: Int32)) && right == -1
            then node
            else makeConst node $ IntVal (left `quot` right)
    Mod -> if right == 0 || (left == (minBound :: Int32)) && right == -1
            then node
            else makeConst node $ IntVal (left `rem` right)
    Lt -> if left < right then makeConst node (BoolVal True) else makeConst node (BoolVal False)
    Leq -> if left <= right then makeConst node (BoolVal True) else makeConst node (BoolVal False)
    Gt -> if left > right then makeConst node (BoolVal True) else makeConst node (BoolVal False)
    Geq -> if left >= right then makeConst node (BoolVal True) else makeConst node (BoolVal False)
    Eq -> if left == right then makeConst node (BoolVal True) else makeConst node (BoolVal False)
    Neq -> if left /= right then makeConst node (BoolVal True) else makeConst node (BoolVal False)
    And -> error "'And' should be handled by short circuting"
    Or -> error "'Or' should be handled by short circuting"
    BitAnd -> makeConst node $ IntVal (left .&. right)
    BitOr -> makeConst node $ IntVal (left .|. right)
    BitXor -> makeConst node $ IntVal (left `xor` right)
    Shl -> makeConst node $ IntVal (shiftLx86 left (fromIntegral right))
    Shr -> makeConst node $ IntVal (shiftRx86 left (fromIntegral right))
    
    
makeConst :: Node -> Value -> Node
makeConst node val = node { nType = ConstNode val }

shiftLx86 :: Int32 -> Int -> Int32
shiftLx86 val amt = shiftL val (amt .&. 0x1F)

shiftRx86 :: Int32 -> Int -> Int32
shiftRx86 val amt = shiftR val (amt .&. 0x1F)
