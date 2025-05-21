{-# LANGUAGE DuplicateRecordFields #-}
module Compile.IR
  ( IRGraph(..),
    Block,
    Node(..),
    NodeId,
    Op(..),
    topoSort,
    irTranslate
  ) where
  
import           Compile.AST (AST)
import qualified Compile.AST as AST

import           Control.Monad.State
import           Control.Monad (mapM_, when)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.Maybe (maybeToList)

import           Text.Megaparsec (SourcePos(..))
import           Text.Megaparsec.Pos (pos1)


type NodeId = Int
type BlockId = Int

data Block = Block
  { name :: String
  , bid :: BlockId
  } deriving (Eq, Show)

data Node
  = Start { nid :: NodeId, block :: Block }
  | Return { nid :: NodeId, expr :: NodeId, sideEffect :: Maybe NodeId, block :: Block }
  | Const { nid :: NodeId, value :: ConstVal, block :: Block }
  | BinaryOp { nid :: NodeId, op :: Op, left :: NodeId, right :: NodeId, sideEffect :: Maybe NodeId, block :: Block }
  | UnaryOp { nid :: NodeId, op :: Op, expr :: NodeId, block :: Block }
  deriving (Eq, Show)
  
data Op
  = Mul
  | Add
  | Sub
  | Div
  | Mod
  | Neg
  deriving (Eq, Show)
  
type ConstVal = String

data TranslationState = TranslationState
  { currentBlock :: Block
  , nextNodeId :: NodeId
  , successors :: IntMap IntSet
  , nodes :: IntMap Node
  , returns :: IntSet
  , currentSideEffect :: NodeId
  , varMapping :: Map String Node
  }

type IRTranslation a = State TranslationState a

data IRGraph = IRGraph
  { irSuccessors :: IntMap IntSet
  , irNodes :: IntMap Node
  , irEntry :: NodeId
  , irReturns :: IntSet
  } deriving (Eq, Show)

irTranslate :: String -> AST -> IRGraph
irTranslate name ast = IRGraph (successors finalState) (nodes finalState) (nid initNode) (returns finalState)
  where
    irTranslation = translateAst ast
    initBlock = Block name 0
    initNode = Start 0 initBlock
    initialState = TranslationState initBlock 1 IntMap.empty (IntMap.singleton 0 initNode) IntSet.empty 0 Map.empty
    finalState = execState irTranslation initialState
    
freshId :: IRTranslation NodeId
freshId = do
  newId <- gets nextNodeId
  modify $ \s -> s { nextNodeId = newId + 1 }
  pure newId
  
addSucc :: Node -> Node -> IRTranslation ()
addSucc from to = modify $ \s -> s
  { successors = IntMap.insertWith IntSet.union (nid from) (IntSet.singleton (nid to)) (successors s) }

addNode :: Node -> IRTranslation ()
addNode n = modify $ \s -> s { nodes = IntMap.insert (nid n) n (nodes s) }

mapVar :: String -> Node -> IRTranslation ()
mapVar ident node = modify $ \s -> s { varMapping = Map.insert ident node (varMapping s) }

translateAst :: AST -> IRTranslation ()
translateAst (AST.Block stmts _) = mapM_ translateStmt stmts
    
translateStmt :: AST.Stmt -> IRTranslation ()
translateStmt (AST.Decl _ _) = pure ()
translateStmt (AST.Init ident expr _) = translateAsgn ident expr
translateStmt (AST.Asgn ident Nothing expr _) = translateAsgn ident expr
translateStmt (AST.Asgn ident (Just op) expr sp) = translateAsgn ident (AST.BinExpr op (AST.Ident ident sp) expr)
translateStmt (AST.Ret expr _) = do
  e <- translateExpr expr
  n <- freshId
  blk <- gets currentBlock
  sideEffect <- gets currentSideEffect
  let ret = Return n (nid e) (Just sideEffect) blk
  modify $ \s -> s { returns = IntSet.insert n (returns s)}
  addSucc e ret
  addNode ret
  
translateAsgn :: String -> AST.Expr -> IRTranslation ()
translateAsgn ident expr = do
  e <- translateExpr expr
  mapVar ident e

translateExpr :: AST.Expr -> IRTranslation Node
translateExpr (AST.IntExpr i _) = do
  n <- freshId
  blk <- gets currentBlock
  let node = Const n i blk
  addNode node
  pure node
translateExpr (AST.Ident ident _) = gets ((Map.! ident) . varMapping)
translateExpr (AST.UnExpr op expr) = do
  i <- freshId
  e <- translateExpr expr
  blk <- gets currentBlock
  let unOp = translateOp op
      node = UnaryOp i unOp (nid e) blk
  addSucc e node
  addNode node
  pure node
translateExpr (AST.BinExpr op expr1 expr2) = do
  i <- freshId
  e1 <- translateExpr expr1
  e2 <- translateExpr expr2
  blk <- gets currentBlock
  curSideEffect <- gets currentSideEffect
  let binOp = translateOp op
      sideEffect = if hasSideEffect binOp then Just curSideEffect else Nothing
      node = BinaryOp i binOp (nid e1) (nid e2) sideEffect blk
  addSucc e1 node
  addSucc e2 node
  addNode node
  when (hasSideEffect binOp) $ setSideEffect i
  pure node
  
setSideEffect :: NodeId -> IRTranslation ()
setSideEffect n = modify $ \s -> s { currentSideEffect = n }

hasSideEffect :: Op -> Bool
hasSideEffect op = case op of
  Div -> True
  Mod -> True
  _   -> False

translateOp :: AST.Op -> Op
translateOp op = case op of
  AST.Mul -> Mul
  AST.Add -> Add
  AST.Sub -> Sub
  AST.Div -> Div
  AST.Neg -> Neg
  AST.Mod -> Mod
  _ -> error ("unknown operator: " ++ show op)


topoSort :: IRGraph -> [NodeId]
topoSort graph = reverse finalOrder
  where
    (_, finalOrder) = foldl visit (Set.empty, []) (IntSet.toList (irReturns graph))
    
    visit (vis, ord) nid
      | nid `Set.member` vis  = (vis, ord)
      | otherwise             = dfs vis ord nid
      
    dfs vis ord nid = (newVis, nid : newOrd)
      where
        vis' = Set.insert nid vis
        node = (irNodes graph) IntMap.! nid
        
        preds = case node of
          (Start _ _)           -> []
          (Return _ e s _)        -> e : maybeToList s
          (Const _ _ _)         -> []
          (BinaryOp _ _ l r s _)  -> l : r : maybeToList s
          (UnaryOp _ _ e _)     -> [e]
        
        (newVis, newOrd) = foldl processPred (vis', ord) preds
        
        processPred (v, o) pid
          | pid `Set.member` v  = (v, o)
          | otherwise           = dfs v o pid