module Compile.IR.SSA
  ( irTranslate
  ) where
  
import           Compile.Frontend.AST (AST)
import qualified Compile.Frontend.AST as AST
import           Compile.IR.IRGraph
import           Compile.IR.GraphConstructor

import Control.Monad.State
import Control.Monad (when)
import Numeric (readHex, readDec)
import Data.Char (isDigit)
import Data.List (isPrefixOf)



irTranslate :: AST -> IRGraph
irTranslate function = graph $ execState (translateFunction function) initialState
  where
    initialState = emptyState
    
translateFunction :: AST -> GraphConstructor ()
translateFunction (AST.Function body) = do
  start <- nid <$> newStart
  seProj <- nid <$> newProj start SideEffect
  writeCurrentSideEffect seProj
  
  blk <- gets currentBlock
  endBlk <- gets (endBlock . graph)
  setCurrentBlock endBlk
  newExit
  setCurrentBlock blk
  
  translateBlock body
    
translateBlock :: AST.Block -> GraphConstructor ()
translateBlock (AST.Block stmts _)= mapUntilRet translateStmt stmts
  where
    mapUntilRet f [] = pure ()
    mapUntilRet f (s:ss) = do
      f s
      case s of
        AST.Ret _ _ -> pure ()
        _ -> mapUntilRet f ss

translateStmt :: AST.Stmt -> GraphConstructor ()
translateStmt stmt = case stmt of
  AST.Decl ty name _ -> pure ()
  
  AST.Init ty name expr _ -> do
    rhs <- translateExpr expr
    blk <- gets currentBlock
    writeVar name blk rhs
    
  AST.Asgn name op expr _ -> do
    blk <- gets currentBlock
    case op of
      Nothing -> do
        rhs <- translateExpr expr
        writeVar name blk rhs
      Just binop -> do
        let bop = translateBinOp binop
        if hasSideEffect bop
          then do
            rhs <- translateExpr expr
            lhs <- readVar name blk
            se <- readCurrentSideEffect
            node <- nid <$> newBinOp bop lhs rhs (Just se)
            projRes <- projResultSE node
            writeVar name blk projRes
          else do
            rhs <- translateExpr expr
            lhs <- readVar name blk
            node <- nid <$> newBinOp bop lhs rhs Nothing
            writeVar name blk node
      
    
  AST.Ret expr _ -> do
    blk <- gets currentBlock
    rhs <- translateExpr expr
    se <- readCurrentSideEffect
    ret <- newReturn rhs (Just se)
    endBlk <- gets (endBlock . graph)
    modify $ \s -> s { graph = addPredecessor (graph s) endBlk (nid ret) }
  
  AST.InnerBlock blk _ -> translateBlock blk
  
  AST.If cond thenStmt maybeElse _ -> do
    -- Setup
    blk <- gets currentBlock
    cNode <- translateExpr cond
    ifNode <- nid <$> newIf cNode
    trueProj <- nid <$> newProj ifNode CondTrue
    falseProj <- nid <$> newProj ifNode CondFalse
    sealBlock blk

    -- Then Branch
    thenBlk <- nid <$> newBlock [trueProj]
    setCurrentBlock thenBlk
    sealBlock thenBlk
    translateStmt thenStmt
    thenEnd <- gets currentBlock
    sealBlock thenEnd
    thenJump <- nid <$> newJump

    -- Else Branch
    elseBlk <- nid <$> newBlock [falseProj]
    setCurrentBlock elseBlk
    sealBlock elseBlk
    case maybeElse of
      Just elseStmt -> translateStmt elseStmt
      Nothing -> pure ()
    elseEnd <- gets currentBlock
    sealBlock elseEnd
    elseJump <- nid <$> newJump

    -- Merge
    mergeBlk <- nid <$> newBlock [thenJump, elseJump]
    setCurrentBlock mergeBlk
    sealBlock mergeBlk

  AST.While cond body _ -> do
    blk <- gets currentBlock
    sealBlock blk
    endJump <- nid <$> newJump
    
    whileBlk <- nid <$> newBlock [endJump]
    setCurrentBlock whileBlk
    addContinueTarget whileBlk
    cNode <- translateExpr cond
    ifNode <- nid <$> newIf cNode
    trueProj <- nid <$> newProj ifNode CondTrue
    falseProj <- nid <$> newProj ifNode CondFalse
    
    mergeBlk <- nid <$> newBlock [falseProj]
    addBreakTarget mergeBlk
    
    bodyBlk <- nid <$> newBlock [trueProj]
    sealBlock bodyBlk
    setCurrentBlock bodyBlk
    translateStmt body
    endBlk <- gets currentBlock
    sealBlock endBlk
    loopJump <- nid <$> newJump
    addPredecessor' whileBlk loopJump
    
    removeContinueTarget
    sealBlock whileBlk
    removeBreakTarget
    sealBlock mergeBlk
    setCurrentBlock mergeBlk
    
  
  AST.For maybeInit cond maybeStep body _ -> do
    case maybeInit of
      Just i -> translateStmt i
      Nothing -> pure ()
      
    blk <- gets currentBlock
    sealBlock blk
    initJump <- nid <$> newJump
    
    forBlk <- nid <$> newBlock [initJump]
    mergeBlk <- nid <$> newBlock []
    addBreakTarget mergeBlk
    bodyBlk <- nid <$> newBlock []
    stepBlk <- nid <$> newBlock []
    addContinueTarget stepBlk
    
    setCurrentBlock forBlk
    cNode <- translateExpr cond
    ifNode <- nid <$> newIf cNode
    trueProj <- nid <$> newProj ifNode CondTrue
    falseProj <- nid <$> newProj ifNode CondFalse
    addPredecessor' bodyBlk trueProj
    addPredecessor' mergeBlk falseProj
    
    setCurrentBlock stepBlk
    case maybeStep of
      Just step -> translateStmt step
      Nothing -> pure ()
    stepEnd <- nid <$> newJump
    addPredecessor' forBlk stepEnd
    
    setCurrentBlock bodyBlk
    translateStmt body
    loopEnd <- nid <$> newJump
    addPredecessor' stepBlk loopEnd
    
    sealBlock forBlk
    sealBlock stepBlk
    sealBlock bodyBlk
    sealBlock mergeBlk
    removeBreakTarget
    removeContinueTarget
    setCurrentBlock mergeBlk
    
  AST.Break _ -> do
    break <- nid <$> newJump
    targets <- gets breakTarget
    let target = case targets of
                   [] -> error "break outside loop"
                   (t:ts) -> t
    addPredecessor' target break
    blk <- gets currentBlock
    sealBlock blk
    afterBreak <- nid <$> newBlock []
    setCurrentBlock afterBreak
  
  AST.Continue _ -> do
    continue <- nid <$> newJump
    targets <- gets continueTarget
    let target = case targets of
                   [] -> error "continue outside loop"
                   (t:ts) -> t
    addPredecessor' target continue
    blk <- gets currentBlock
    sealBlock blk
    afterContinue <- nid <$> newBlock []
    setCurrentBlock afterContinue
  
  
translateExpr :: AST.Expr -> GraphConstructor NodeId
translateExpr expr = case expr of
  AST.Ident name _ -> do
    blk <- gets currentBlock
    val <- readVar name blk
    return val
    
  AST.IntExpr str _ -> do
    let val = parseIntLiteral str
    node <- newConst (IntVal val)
    return (nid node)
    
  AST.BoolLit bool _ -> do
    node <- newConst (BoolVal bool)
    return (nid node)
    
  AST.UnExpr op e -> do
    val <- translateExpr e
    let uop = translateUnOp op
    res <- newUnOp uop val
    return (nid res)
    
  AST.BinExpr op e1 e2 -> do
    let bop = translateBinOp op
    case bop of
      And -> translateShortCircuit bop e1 e2
      Or -> translateShortCircuit bop e1 e2
      _ -> do
        blk <- gets currentBlock
        lhs <- translateExpr e1
        rhs <- translateExpr e2
        se <- readCurrentSideEffect
        if hasSideEffect bop
          then do
            node <- nid <$> newBinOp bop lhs rhs (Just se)
            projResultSE node
          else do
            node <- nid <$> newBinOp bop lhs rhs Nothing
            pure node
    
  AST.Ternary cond thenExpr elseExpr -> error "not implemented"
  
translateBinOp :: AST.BinOp -> BinOp
translateBinOp op = case op of
  AST.Mul -> Mul
  AST.Add -> Add
  AST.Sub -> Sub
  AST.Div -> Div
  AST.Mod -> Mod
  AST.Gt  -> Gt
  AST.Lt  -> Lt
  AST.Leq -> Leq
  AST.Geq -> Geq
  AST.Eq -> Eq
  AST.Neq -> Neq
  AST.And -> And
  AST.Or -> Or
  AST.BitAnd -> BitAnd
  AST.BitOr -> BitOr
  AST.BitXor -> BitXor
  AST.Shl -> Shl
  AST.Shr -> Shr
  _ -> error ("Unknown binary operator: " ++ show op)
  
translateUnOp :: AST.UnOp -> UnOp
translateUnOp op = case op of
  AST.Neg -> Neg
  AST.Not -> Not
  AST.BitNot -> BitNot
  _ -> error ("Unknown unary operator: " ++ show op)
  
translateShortCircuit :: BinOp -> AST.Expr -> AST.Expr -> GraphConstructor NodeId
translateShortCircuit op e1 e2 = error "not implemented"

      
projResultSE :: NodeId -> GraphConstructor NodeId
projResultSE n = do
  projSE <- nid <$> newProj n SideEffect
  writeCurrentSideEffect projSE
  nid <$> newProj n Result
  
hasSideEffect :: BinOp -> Bool
hasSideEffect op = case op of
  Div -> True
  Mod -> True
  _ -> False


parseIntLiteral :: String -> Int
parseIntLiteral s
  | "0x" `isPrefixOf` s = readBase readHex (drop 2 s)
  | "0X" `isPrefixOf` s = readBase readHex (drop 2 s)
  | otherwise           = readBase readDec s
  
readBase :: ReadS Int -> String -> Int
readBase parser str = case parser str of
  [(n, "")] -> n
  _         -> error ("Failed to parse integer: " ++ str)
  
addPredecessor' :: NodeId -> NodeId -> GraphConstructor ()
addPredecessor' nodeId predId = modify $ \s -> s { graph = addPredecessor (graph s) nodeId predId }