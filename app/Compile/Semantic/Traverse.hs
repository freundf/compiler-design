module Compile.Semantic.Traverse where

import Compile.Frontend.AST
import Compile.Semantic.Util
import Error (L1ExceptT)

import Control.Monad.State
import Control.Monad (when)

import Text.Megaparsec (SourcePos)

data Handler m = Handler
  { hFuncEnter :: AST -> m ()
  , hFuncExit :: AST -> m ()
  
  , hBlockEnter :: Block -> SourcePos -> m ()
  , hBlockExit :: Block -> SourcePos -> m ()
  
  , hDecl     :: Type -> String -> SourcePos -> m ()
  , hInit     :: Type -> String -> Expr -> SourcePos -> m ()
  , hAsgn     :: String -> AsgnOp -> Expr -> SourcePos -> m ()
  , hRet      :: Expr -> SourcePos -> m ()
  , hIf       :: Expr -> Stmt -> Maybe Stmt -> SourcePos -> m ()
  , hWhile    :: Expr -> Stmt -> SourcePos -> m ()
  , hFor      :: Maybe Stmt -> Expr -> Maybe Stmt -> Stmt -> SourcePos -> m ()
  , hBreak    :: SourcePos -> m ()
  , hContinue :: SourcePos -> m ()
  , hInnerBlock :: Block -> SourcePos -> m ()
  
  , hIdent :: String -> SourcePos -> m ()
  , hBoolLit :: Bool -> SourcePos -> m ()
  , hIntExpr :: String -> SourcePos -> m ()
  , hUnExpr :: UnOp -> Expr -> m ()
  , hBinExpr :: BinOp -> Expr -> Expr -> m ()
  , hTernary :: Expr -> Expr -> Expr -> m ()
  }
  
defaultHandler :: Monad m => Handler m
defaultHandler = Handler
  { hFuncEnter = \_ -> return ()
  , hFuncExit  = \_ -> return ()

  , hBlockEnter    = \_ _ -> return ()
  , hBlockExit     = \_ _ -> return ()

  , hDecl          = \_ _ _ -> return ()
  , hInit          = \_ _ _ _ -> return ()
  , hAsgn          = \_ _ _ _ -> return ()
  , hRet           = \_ _ -> return ()
  , hIf            = \_ _ _ _ -> return ()
  , hWhile         = \_ _ _ -> return ()
  , hFor           = \_ _ _ _ _ -> return ()
  , hBreak         = \_ -> return ()
  , hContinue      = \_ -> return ()
  , hInnerBlock    = \_ _ -> return ()

  , hIdent         = \_ _ -> return ()
  , hBoolLit       = \_ _ -> return ()
  , hIntExpr       = \_ _ -> return ()
  , hUnExpr        = \_ _ -> return ()
  , hBinExpr       = \_ _ _ -> return ()
  , hTernary       = \_ _ _ -> return ()
  }

  
data TraversalOrder = PreOrder | PostOrder
  deriving (Eq, Show)

traverseAST :: TraversalOrder -> Handler (StateT Context L1ExceptT) -> AST -> Semantic ()
traverseAST order handler ast@(Function blk) = do
  hFuncEnter handler ast
  traverseBlock order handler blk
  hFuncExit handler ast

traverseBlock :: TraversalOrder -> Handler (StateT Context L1ExceptT) -> Block -> Semantic ()
traverseBlock order handler blk@(Block stmts pos) = do
  hBlockEnter handler blk pos
  inScope $ mapM_ (traverseStmt order handler) stmts
  hBlockExit handler blk pos

traverseStmt :: TraversalOrder -> Handler (StateT Context L1ExceptT) -> Stmt -> Semantic ()
traverseStmt order handler stmt = case stmt of
  Decl ty name pos -> hDecl handler ty name pos
  
  Init ty name expr pos -> do
    withOrder order (hInit handler ty name expr pos) $
      traverseExpr' expr
    
  Asgn name op expr pos -> do
    withOrder order (hAsgn handler name op expr pos) $
      traverseExpr' expr
    
  Ret expr pos -> do
    withOrder order (hRet handler expr pos) $
      traverseExpr' expr
    
  While cond body pos -> do
    withOrder order (hWhile handler cond body pos) $ do
      traverseExpr' cond
      inLoop $ traverseStmt' body
    
  For mInit cond mStep body pos -> do
    withOrder order (hFor handler mInit cond mStep body pos) $ do
      inScope $ do
        case mInit of
          Just initStmt -> traverseStmt' initStmt
          Nothing -> pure ()
        traverseExpr' cond
        case mStep of
          Just stepStmt -> traverseStmt' stepStmt
          Nothing -> pure ()
        inLoop $ traverseStmt' body
    
  If cond thenStmt mElse pos -> do
    withOrder order (hIf handler cond thenStmt mElse pos) $ do
      traverseExpr' cond
      traverseStmt' thenStmt
      case mElse of
        Just elseStmt -> traverseStmt' elseStmt
        Nothing -> pure ()
    
  Break pos -> hBreak handler pos
  
  Continue pos -> hContinue handler pos
  
  InnerBlock blk pos -> do
    withOrder order (hInnerBlock handler blk pos) $
      traverseBlock' blk
  
  where
    traverseBlock' = traverseBlock order handler
    traverseStmt' = traverseStmt order handler
    traverseExpr' = traverseExpr order handler
    
    
traverseExpr :: TraversalOrder -> Handler (StateT Context L1ExceptT)  -> Expr -> Semantic ()
traverseExpr order handler expr = case expr of
  BoolLit b pos -> hBoolLit handler b pos
  IntExpr s pos -> hIntExpr handler s pos
  Ident name pos -> hIdent handler name pos
  UnExpr op e -> do
    withOrder order (hUnExpr handler op e) $
      traverseExpr' e
  BinExpr op e1 e2 -> do
    withOrder order (hBinExpr handler op e1 e2) $ do
      traverseExpr' e1
      traverseExpr' e2
  Ternary c e1 e2 -> do
    withOrder order (hTernary handler c e1 e2) $ do
      traverseExpr' c
      traverseExpr' e1
      traverseExpr' e2
  
  where
    traverseExpr' = traverseExpr order handler
    
withOrder :: Monad m => TraversalOrder -> m () -> m a -> m a
withOrder order handlerAction body = do
  when (order == PreOrder) $ handlerAction
  result <- body
  when (order == PostOrder) $ handlerAction
  return result
  
combineHandlers :: Monad m => Handler m -> Handler m -> Handler m
combineHandlers h1 h2 = Handler
  { hFuncEnter     = \blk -> hFuncEnter h1 blk >> hFuncEnter h2 blk
  , hFuncExit      = \blk -> hFuncExit h1 blk >> hFuncExit h2 blk
  , hBlockEnter    = \blk -> hBlockEnter h1 blk >> hBlockEnter h2 blk
  , hBlockExit     = \blk -> hBlockExit h1 blk >> hBlockExit h2 blk
  , hDecl          = \ty name pos -> hDecl h1 ty name pos >> hDecl h2 ty name pos
  , hInit          = \ty name expr pos -> hInit h1 ty name expr pos >> hInit h2 ty name expr pos
  , hAsgn          = \name op expr pos -> hAsgn h1 name op expr pos >> hAsgn h2 name op expr pos
  , hRet           = \expr pos -> hRet h1 expr pos >> hRet h2 expr pos
  , hIf            = \cond pos -> hIf h1 cond pos >> hIf h2 cond pos
  , hWhile         = \cond pos -> hWhile h1 cond pos >> hWhile h2 cond pos
  , hFor           = \mI cond mS pos -> hFor h1 mI cond mS pos >> hFor h2 mI cond mS pos
  , hBreak         = \pos -> hBreak h1 pos >> hBreak h2 pos
  , hContinue      = \pos -> hContinue h1 pos >> hContinue h2 pos
  , hIdent         = \name pos -> hIdent h1 name pos >> hIdent h2 name pos
  , hBoolLit       = \b pos -> hBoolLit h1 b pos >> hBoolLit h2 b pos
  , hIntExpr       = \s pos -> hIntExpr h1 s pos >> hIntExpr h2 s pos
  , hUnExpr        = \op e -> hUnExpr h1 op e >> hUnExpr h2 op e
  , hBinExpr       = \op e1 e2 -> hBinExpr h1 op e1 e2 >> hBinExpr h2 op e1 e2
  , hTernary       = \c e1 e2 -> hTernary h1 c e1 e2 >> hTernary h2 c e1 e2
  , hInnerBlock    = \b pos -> hInnerBlock h1 b pos >> hInnerBlock h2 b pos
  }


chainHandlers :: Monad m => [Handler m] -> Handler m
chainHandlers = foldl combineHandlers defaultHandler