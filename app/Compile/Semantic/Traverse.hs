module Compile.Semantic.Traverse where

import Compile.Frontend.AST
import Compile.Semantic.Util
import Error (L1ExceptT)

import Control.Monad.State.Strict
import Control.Monad (when)
import Data.List (foldl')

import Text.Megaparsec (SourcePos)

data Handler m = Handler
  { hAST :: AST -> m ()
  , hFuncEnter :: Function -> m ()
  , hFuncExit :: Function -> m ()
  
  , hBlockEnter :: Block -> SourcePos -> m ()
  , hBlockExit :: Block -> SourcePos -> m ()
  
  , hDecl     :: Type -> String -> SourcePos -> m ()
  , hInit     :: Type -> String -> Expr -> SourcePos -> m ()
  , hAsgn     :: String -> AsgnOp -> Expr -> SourcePos -> m ()
  , hRet      :: Expr -> SourcePos -> m ()
  , hIf       :: Expr -> Stmt -> Maybe Stmt -> SourcePos -> m ()
  , hIfThen   :: Stmt -> m ()
  , hIfElse   :: Maybe Stmt -> m ()
  , hWhile    :: Expr -> Stmt -> SourcePos -> m ()
  , hFor      :: Maybe Stmt -> Expr -> Maybe Stmt -> Stmt -> SourcePos -> m ()
  , hBreak    :: SourcePos -> m ()
  , hContinue :: SourcePos -> m ()
  , hInnerBlock :: Block -> SourcePos -> m ()
  , hCall :: String -> [Expr] -> SourcePos -> m ()
  
  , hIdent :: String -> SourcePos -> m ()
  , hBoolLit :: Bool -> SourcePos -> m ()
  , hIntExpr :: String -> SourcePos -> m ()
  , hUnExpr :: UnOp -> Expr -> m ()
  , hBinExpr :: BinOp -> Expr -> Expr -> m ()
  , hTernary :: Expr -> Expr -> Expr -> m ()
  , hCallExpr :: String -> [Expr] -> SourcePos -> m ()
  }
  
defaultHandler :: Monad m => Handler m
defaultHandler = Handler
  { hAST = \_ -> return ()
  , hFuncEnter = \_ -> return ()
  , hFuncExit  = \_ -> return ()

  , hBlockEnter    = \_ _ -> return ()
  , hBlockExit     = \_ _ -> return ()

  , hDecl          = \_ _ _ -> return ()
  , hInit          = \_ _ _ _ -> return ()
  , hAsgn          = \_ _ _ _ -> return ()
  , hRet           = \_ _ -> return ()
  , hIf            = \_ _ _ _ -> return ()
  , hIfThen        = \_ -> return ()
  , hIfElse        = \_ -> return ()
  , hWhile         = \_ _ _ -> return ()
  , hFor           = \_ _ _ _ _ -> return ()
  , hBreak         = \_ -> return ()
  , hContinue      = \_ -> return ()
  , hInnerBlock    = \_ _ -> return ()
  , hCall          = \_ _ _ -> return ()

  , hIdent         = \_ _ -> return ()
  , hBoolLit       = \_ _ -> return ()
  , hIntExpr       = \_ _ -> return ()
  , hUnExpr        = \_ _ -> return ()
  , hBinExpr       = \_ _ _ -> return ()
  , hTernary       = \_ _ _ -> return ()
  , hCallExpr          = \_ _ _ -> return ()
  }


class Monad m => TraverseMonad m where
   inScope :: m a -> m a
   inScope_ :: m a -> m a
   inLoop :: m a -> m a

data TraversalOrder = PreOrder | PostOrder
  deriving (Eq, Show)

traverseAST :: TraverseMonad m => TraversalOrder -> Handler m -> AST -> m ()
traverseAST order handler functions = do
  withOrder order (hAST handler functions) $
    mapM_ (traverseFunction order handler) functions

traverseFunction :: TraverseMonad m => TraversalOrder -> Handler m -> Function -> m ()
traverseFunction order handler f@(Function retTy name params blk pos) = do
  hFuncEnter handler f
  traverseBlock order handler blk
  hFuncExit handler f

traverseBlock :: TraverseMonad m => TraversalOrder -> Handler m -> Block -> m ()
traverseBlock order handler blk@(Block stmts pos) = do
  hBlockEnter handler blk pos
  inScope $ mapM_ (traverseStmt order handler) stmts
  hBlockExit handler blk pos

traverseStmt :: TraverseMonad m => TraversalOrder -> Handler m -> Stmt -> m ()
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
    inScope $ do
      withOrder order (hFor handler mInit cond mStep body pos) $ do
        case mInit of
          Just initStmt -> traverseStmt' initStmt
          Nothing -> pure ()
        traverseExpr' cond
        inLoop $ traverseStmt' body
        case mStep of
          Just stepStmt -> traverseStmt' stepStmt
          Nothing -> pure ()
    
  If cond thenStmt mElse pos -> do
    withOrder order (hIf handler cond thenStmt mElse pos) $ do
      traverseExpr' cond
      inScope_ $ do
        withOrder order (hIfThen handler thenStmt) $ traverseStmt' thenStmt
      inScope_ $
        withOrder order (hIfElse handler mElse) $ do
          case mElse of
            Just elseStmt -> traverseStmt' elseStmt
            Nothing -> pure ()
    
  Break pos -> hBreak handler pos
  
  Continue pos -> hContinue handler pos
  
  InnerBlock blk pos -> do
    withOrder order (hInnerBlock handler blk pos) $
      traverseBlock' blk

  Call func params pos -> do
    withOrder order (hCall handler func params pos) $
      mapM_ traverseExpr' params
  
  where
    traverseBlock' = traverseBlock order handler
    traverseStmt' = traverseStmt order handler
    traverseExpr' = traverseExpr order handler
    
    
traverseExpr :: TraverseMonad m => TraversalOrder -> Handler m -> Expr -> m ()
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
  CallExpr func params pos -> do
    withOrder order (hCallExpr handler func params pos) $ do
      mapM_ traverseExpr' params
  where
    traverseExpr' = traverseExpr order handler
    
withOrder :: TraverseMonad m => TraversalOrder -> m () -> m a -> m a
withOrder order handlerAction body = do
  when (order == PreOrder) $ handlerAction
  result <- body
  when (order == PostOrder) $ handlerAction
  return result
  
combineHandlers :: Monad m => Handler m -> Handler m -> Handler m
combineHandlers h1 h2 = Handler
  { hAST           = \ast -> hAST h1 ast >> hAST h2 ast
  , hFuncEnter     = \blk -> hFuncEnter h1 blk >> hFuncEnter h2 blk
  , hFuncExit      = \blk -> hFuncExit h1 blk >> hFuncExit h2 blk
  , hBlockEnter    = \blk pos -> hBlockEnter h1 blk pos >> hBlockEnter h2 blk pos
  , hBlockExit     = \blk pos -> hBlockExit h1 blk pos >> hBlockExit h2 blk pos
  , hDecl          = \ty name pos -> hDecl h1 ty name pos >> hDecl h2 ty name pos
  , hInit          = \ty name expr pos -> hInit h1 ty name expr pos >> hInit h2 ty name expr pos
  , hAsgn          = \name op expr pos -> hAsgn h1 name op expr pos >> hAsgn h2 name op expr pos
  , hRet           = \expr pos -> hRet h1 expr pos >> hRet h2 expr pos
  , hIf            = \cond thenStmt mElse pos -> hIf h1 cond thenStmt mElse pos >> hIf h2 cond thenStmt mElse pos
  , hIfThen        = \thenStmt -> hIfThen h1 thenStmt >> hIfThen h2 thenStmt
  , hIfElse        = \mElse -> hIfElse h1 mElse >> hIfElse h2 mElse
  , hWhile         = \cond body pos -> hWhile h1 cond body pos >> hWhile h2 cond body pos
  , hFor           = \mI cond mS body pos -> hFor h1 mI cond mS body pos >> hFor h2 mI cond mS body pos
  , hBreak         = \pos -> hBreak h1 pos >> hBreak h2 pos
  , hContinue      = \pos -> hContinue h1 pos >> hContinue h2 pos
  , hIdent         = \name pos -> hIdent h1 name pos >> hIdent h2 name pos
  , hBoolLit       = \b pos -> hBoolLit h1 b pos >> hBoolLit h2 b pos
  , hIntExpr       = \s pos -> hIntExpr h1 s pos >> hIntExpr h2 s pos
  , hUnExpr        = \op e -> hUnExpr h1 op e >> hUnExpr h2 op e
  , hBinExpr       = \op e1 e2 -> hBinExpr h1 op e1 e2 >> hBinExpr h2 op e1 e2
  , hTernary       = \c e1 e2 -> hTernary h1 c e1 e2 >> hTernary h2 c e1 e2
  , hInnerBlock    = \b pos -> hInnerBlock h1 b pos >> hInnerBlock h2 b pos
  , hCall          = \f ps pos -> hCall h1 f ps pos >> hCall h2 f ps pos
  , hCallExpr      = \f ps pos -> hCallExpr h1 f ps pos >> hCallExpr h2 f ps pos
  }


chainHandlers :: Monad m => [Handler m] -> Handler m
chainHandlers = foldl' combineHandlers defaultHandler