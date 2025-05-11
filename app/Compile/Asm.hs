module Compile.Asm
  ( codeGen
  ) where
  
import           Compile.X86
import           Compile.AST (AST(..), Stmt, Expr(..))
import qualified Compile.AST as AST
import           Compile.RegAlloc (regAlloc, naiveStrategy)

import           Control.Monad.State
import qualified Data.Map as Map

type VarName = String

type CodeGen a = State CodeGenState a

data CodeGenState = CodeGenState
  { regMap :: Map.Map VarName Opnd
  , nextReg :: Integer
  , code :: X86
  }

codeGen :: AST -> X86
codeGen (Block stmts _) = Prologue : regAlloc (code finalState) strategy
  where
    initialState = CodeGenState Map.empty 0 []
    finalState = execState (genBlock stmts) initialState
    strategy = naiveStrategy (nextReg finalState)
    
freshReg :: CodeGen Opnd
freshReg = do
  n <- gets nextReg
  modify $ \s -> s {nextReg = n + 1}
  pure (VirtReg n)
  
assignVar :: VarName -> Opnd -> CodeGen ()
assignVar name r = do
  modify $ \s -> s { regMap = Map.insert name r (regMap s)}
  
lookupVar :: VarName -> CodeGen Opnd
lookupVar name = do
  m <- gets regMap
  case Map.lookup name m of
    Just r -> pure r
    Nothing -> error "unreachable, error in semantic analysis"
    
emit :: Instr -> CodeGen()
emit instr = modify $ \s -> s { code = (code s) ++ [instr]}

genBlock :: [Stmt] -> CodeGen ()
genBlock = mapM_ genStmt

genStmt :: Stmt -> CodeGen ()
genStmt (AST.Decl name _) = do
  r <- freshReg
  assignVar name r
genStmt (AST.Init name e _) = do
  r <- genExpr e
  assignVar name r
genStmt (AST.Asgn name op e _) = do
  rhs <- genExpr e
  lhs <- lookupVar name
  emit (Mov lhs rhs)
genStmt (AST.Ret e _) = do
  r <- genExpr e
  emit (Mov (Reg RAX) r)
  emit (Ret)
  
genExpr :: Expr -> CodeGen Opnd
genExpr (IntExpr n _) = do
  r <- freshReg
  emit (Mov r (Imm (read n)))
  pure r
genExpr (Ident name _) = lookupVar name
genExpr (UnExpr op e) = do
  r <- genExpr e
  emit $ case op of
    AST.Neg -> (Neg r)
    _ -> error ("unknown unary expression: " ++ show op)
  pure r
genExpr (BinExpr op e1 e2) = do
  r1 <- genExpr e1
  r2 <- genExpr e2
  r <- freshReg
  case op of
    AST.Mul -> do
      emit (Mov r r1)
      emit (Imul r r2)
    AST.Add -> do
      emit (Mov r r1)
      emit (Add r r2)
    AST.Sub -> do
      emit (Mov r r1)
      emit (Sub r r2)
    AST.Div -> do
      emit (Mov (Reg RAX) r1)
      emit Cqo
      emit (Idiv r2)
      emit (Mov r (Reg RAX))
    AST.Mod -> do
      emit (Mov (Reg RAX) r1)
      emit Cqo
      emit (Idiv r2)
      emit (Mov r (Reg RDX))
  pure r
  