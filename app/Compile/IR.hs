module Compile.IR
  ( genIR
  ) where
  
import           Compile.AST (AST, Op)
import qualified Compile.AST as AST

import           Control.Monad.State
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type NodeID = Int
type BlockID = Int

data Function = Function
  { entryBlock  :: BlockID
  , blocks      :: IntMap BasicBlock
  , graph       :: IRGraph
  }
  deriving (Show)
  
data BasicBlock = BasicBlock
  { blockId     :: BlockID
  , phiNodes    :: [Node]
  , bodyNodes   :: [Node]
  , terminator  :: Node
  }
  deriving (Show)
  
data Node = Node
  { nodeId :: NodeID
  , nodeKind :: NodeKind
  }
  deriving (Show)
  
data NodeKind
  = IntConst Integer
  | Param String
  | UnOp Op NodeID
  | BinOp Op NodeID NodeID
  | Phi [(BlockID, NodeID)]
  | Return NodeID
  | Start
  deriving (Show)
  
  
type IRGraph = IntMap Node

  
data IRState = IRState
  { nextId  :: Int
  , irGraph   :: IRGraph
  , irBlocks  :: IntMap BasicBlock
  , current :: BlockID
  , varEnv  :: Map String NodeID
  }
  
type IRGen = State IRState

initialState :: IRState
initialState = IRState
  { nextId = 1
  , irGraph = IntMap.empty
  , irBlocks = IntMap.singleton 0 (BasicBlock 0 [] [] (Node 0 Start))
  , current = 0
  , varEnv = Map.empty
  }

buildGraph :: IRGen () -> Function
buildGraph m = Function
  { entryBlock  = 0
  , blocks      = irBlocks endState
  , graph       = irGraph endState
  }
  where
    endState = execState m initialState
    
    
freshId :: IRGen NodeID
freshId = do
  n <- gets nextId
  modify $ \s -> s { nextId = n + 1 }
  pure n
  

emit :: NodeKind -> IRGen NodeID
emit kind = do
  id <- freshId
  let node = Node id kind
  modify $ \s -> s { irGraph = IntMap.insert id node (irGraph s)}
  blockId <- gets current
  modify $ \s ->
    let basicBlock = (irBlocks s) IntMap.! blockId
        basicBlock' = basicBlock { bodyNodes = bodyNodes basicBlock ++ [node] }
    in s { irBlocks = IntMap.insert blockId basicBlock' (irBlocks s)}
  pure id

    
translateStmt :: AST.Stmt -> IRGen ()
translateStmt (AST.Decl name _) = translateStmt (AST.Init name (AST.IntExpr 0 undefined) undefined)
translateStmt (AST.Init name expr _) = do
  e <- translateExpr expr
  modify $ \s -> s { varEnv = Map.insert name e (varEnv s)}
translateStmt (AST.Asgn name op expr _) = do
  rhs <- translateExpr expr
  env <- gets varEnv
  let old = Map.findWithDefault (error "assign to undeclared") name env
  newVal <- case op of
    Nothing -> pure rhs
    Just op -> emit (BinOp op old rhs)
  modify $ \s -> s { varEnv = Map.insert name newVal (varEnv s)}
translateStmt (AST.Ret expr _) = do
  e <- translateExpr expr
  id <- freshId
  let retNode = Node id (Return e)
  modify $ \s -> s { irGraph = IntMap.insert id retNode (irGraph s)}
  blockId <- gets current
  modify $ \s ->
    let basicBlock = (irBlocks s) IntMap.! blockId
        basicBlock' = basicBlock { terminator = retNode }
    in s { irBlocks = IntMap.insert blockId basicBlock' (irBlocks s)}
  
  
translateExpr :: AST.Expr -> IRGen NodeID
translateExpr (AST.IntExpr i _) = emit (IntConst i)
translateExpr (AST.Ident v _) = do
  env <- gets varEnv
  case Map.lookup v env of
    Just id -> pure id
    Nothing -> error $ "variable not declared: " ++ v
translateExpr (AST.UnExpr op expr) = do
  e <- translateExpr expr
  emit (UnOp op e)
translateExpr (AST.BinExpr op expr1 expr2) = do
  e1 <- translateExpr expr1
  e2 <- translateExpr expr2
  emit (BinOp op e1 e2)


genIR :: AST -> Function
genIR (AST.Block stmts _) = buildGraph $ mapM_ translateStmt stmts