module Compile.IR.SSA
  (
  ) where
  
import           Compile.Frontend.AST (AST)
import qualified Compile.Frontend.AST as AST
import           Compile.IR.IRGraph
import           Compile.IR.GraphConstructor
import           Compile.IR.Util.YCompPrinter

import Control.Monad.State
import Control.Monad (when)
import Numeric (readHex, readDec)
import Data.Char (isDigit)
import Data.List (isPrefixOf)


irTranslate :: AST -> IRGraph
irTranslate (AST.Function blk) = graph $ execState (translateBlock blk) initialState
  where
    initialState = emptyState
    
    
translateBlock :: AST.Block -> GraphConstructor ()
translateBlock (AST.Block stmts _)= mapUntilRet translateStmt stmts
  where
    mapUntilRet f [] = sealLast
    mapUntilRet f (s:ss) = do
      f s
      case s of
        AST.Ret _ _ -> sealLast
        _ -> mapUntilRet f ss
        
    sealLast = do
      blk <- gets currentBlock
      sealBlock blk

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
        lhs <- readVar name blk
        rhsExpr <- translateExpr expr
        se <- readCurrentSideEffect
        let nodeSe = if hasSideEffect bop then Just se else Nothing
        node <- newBinOp bop lhs rhsExpr nodeSe
        when (hasSideEffect bop) $ writeCurrentSideEffect (nid node)
        writeVar name blk (nid node)
      
    
  AST.Ret expr _ -> do
    blk <- gets currentBlock
    rhs <- translateExpr expr
    se <- readCurrentSideEffect
    ret <- newReturn rhs (Just se)
    endBlk <- gets (endBlock . graph)
    modify $ \s -> s { graph = addPredecessor (graph s) endBlk (nid ret) }
  
  AST.InnerBlock blk _ -> do
    prevBlock <- gets currentBlock
    innerBlock <- nid <$> newBlock [prevBlock]
    setCurrentBlock innerBlock
    sealBlock innerBlock
    translateBlock blk
    nextBlock <- nid <$> newBlock [innerBlock]
    setCurrentBlock nextBlock
    sealBlock nextBlock
       
  AST.If cond thenStmt maybeElse _ -> do
    -- Setup
    condVal <- translateExpr cond
    blk <- gets currentBlock
    thenBlk <- nid <$> newBlock [blk]
    elseBlk <- nid <$> newBlock [blk]
    mergeBlk <- nid <$> newBlock []
    newBranch condVal thenBlk elseBlk
    
    -- Then
    setCurrentBlock thenBlk
    mapM_ translateStmt thenStmt
    thenEnd <- gets currentBlock
    newJump mergeBlk
    sealBlock thenEnd
    
    -- Else
    setCurrentBlock elseBlk
    case maybeElse of
      Just elseStmt -> mapM_ translateStmt elseStmt
      Nothing -> pure ()
    elseEnd <- gets currentBlock
    newJump mergeBlk
    sealBlock elseEnd
    
    -- Merge
    setCurrentBlock mergeBlk
    sealBlock mergeBlk
    
  AST.While cond body _ -> do
    blk <- gets currentBlock
    
    -- loop header
    loopHeader <- nid <$> newBlock [blk]
    setCurrentBlock loopHeader
    condVal <- translateExpr cond
    
    -- loop
    loopBody <- nid <$> newBlock [loopHeader]
    exitBlk <- nid <$> newBlock [loopHeader]
    newBranch condVal loopBody exitBlk
    
    oldBreak <- gets breakTarget
    oldContinue <- gets continueTarget
    modify $ \s -> s { breakTarget = Just exitBlk  , continueTarget = Just loopHeader }
    
    setCurrentBlock loopBody
    mapM_ translateStmt body
    bodyEnd <- gets currentBlock
    newJump loopHeader
    sealBlock bodyEnd
    sealBlock loopHeader
    
    modify $ \s -> s { breakTarget = oldBreak, continueTarget = oldContinue }
    
    -- End Block
    setCurrentBlock exitBlk
    sealBlock exitBlk
    
  AST.For maybeInit cond maybeStep body _ -> do
    case maybeInit of
      Just initStmt -> translateStmt initStmt
      Nothing       -> pure ()
    
    beforeBlk <- gets currentBlock
    loopHeader <- nid <$> newBlock [beforeBlk]
    setCurrentBlock loopHeader
    condVal <- translateExpr cond
    
    bodyBlk <- nid <$> newBlock [loopHeader]
    exitBlk <- nid <$> newBlock [loopHeader]
    newBranch condVal bodyBlk exitBlk
    
    oldBreak <- gets breakTarget
    oldContinue <- gets continueTarget
    modify $ \s -> s { breakTarget = Just exitBlk  , continueTarget = Just loopHeader }
    
    setCurrentBlock bodyBlk
    mapM_ translateStmt body
    
    case maybeStep of
      Just stepStmt -> translateStmt stepStmt
      Nothing       -> pure ()
    
    bodyEnd <- gets currentBlock
    newJump loopHeader
    sealBlock bodyEnd
    sealBlock loopHeader
    
    modify $ \s -> s { breakTarget = oldBreak, continueTarget = oldContinue }
    
    setCurrentBlock exitBlk
    sealBlock exitBlk
    
  AST.Break _ -> do
    mBreak <- gets breakTarget
    case mBreak of
      Just breakBlk -> do
        newJump breakBlk
        blk <- gets currentBlock
        sealBlock blk
      Nothing -> error "Break outside loop"
      
  AST.Continue _ -> do
    mContinue <- gets continueTarget
    case mContinue of
      Just continueBlk -> do
        newJump continueBlk
        blk <- gets currentBlock
        sealBlock blk
      Nothing -> error "Continue outside loop"
      
  
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
    blk <- gets currentBlock
    lhs <- translateExpr e1
    rhs <- translateExpr e2
    se <- readCurrentSideEffect
    let bop = translateBinOp op
        nodeSe = if hasSideEffect bop then Just se else Nothing
    res <- newBinOp bop lhs rhs nodeSe
    if hasSideEffect bop
      then do
        projSe <- newProj (nid res) SideEffect
        writeCurrentSideEffect (nid projSe)
        projRes <- newProj (nid res) Result
        return (nid projRes)
      else
        return (nid res)
    
  AST.Ternary cond thenExpr elseExpr -> do
    blk <- gets currentBlock
    condVal <- translateExpr cond
    
    thenBlk <- nid <$> newBlock [blk]
    elseBlk <- nid <$> newBlock [blk]
    mergeBlk <- nid <$> newBlock []
    
    newBranch condVal thenBlk elseBlk
    
    setCurrentBlock thenBlk
    thenVal <- translateExpr thenExpr
    newJump mergeBlk
    thenEnd <- gets currentBlock
    sealBlock thenEnd
    
    setCurrentBlock elseBlk
    elseVal <- translateExpr elseExpr
    newJump mergeBlk
    elseEnd <- gets currentBlock
    sealBlock elseEnd
    
    setCurrentBlock mergeBlk
    sealBlock mergeBlk
    
    phi <- newPhi mergeBlk [thenVal, elseVal]
    return (nid phi)
  
translateBinOp :: AST.BinOp -> BinOp
translateBinOp op = case op of
  AST.Mul -> Mul
  AST.Add -> Add
  AST.Sub -> Sub
  AST.Div -> Div
  AST.Mod -> Mod
  AST.Gt  -> Gt
  AST.Lt  -> Lt
  _ -> error ("Unknown binary operator: " ++ show op)
  
translateUnOp :: AST.UnOp -> UnOp
translateUnOp op = case op of
  AST.Neg -> Neg
  _ -> error ("Unknown unary operator: " ++ show op)
  
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