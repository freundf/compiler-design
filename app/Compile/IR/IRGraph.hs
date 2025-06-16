module Compile.IR.IRGraph
  ( IRGraph(..)
  , Node(..)
  , NodeType(..)
  , NodeId
  , Name
  , ProjInfo(..)
  , BinOp(..)
  , UnOp(..)
  , Value(..)
  , newGraph
  , predecessors
  , addPredecessor
  , setPredecessor
  , addSuccessor
  , addNode
  , isTerminator
  , getBlocks
  , getNode
  , intVal
  ) where

  
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.List (intercalate)
import           Data.Int (Int32)


type NodeId = Int
type Name = String
data ProjInfo = Result | SideEffect | CondTrue | CondFalse
  deriving (Eq, Show)

data Node = Node
  { nid :: NodeId
  , block :: NodeId
  , nType :: NodeType
  } deriving (Eq, Show)
 
data NodeType
  = Start
  | Block { preds :: [NodeId], label :: String }
  | Return { expr :: NodeId, sideEffect :: Maybe NodeId }
  | ConstNode { value :: Value }
  | BinOpNode { binOp :: BinOp, left :: NodeId, right :: NodeId, sideEffect :: Maybe NodeId }
  | UnOpNode { unOp :: UnOp, expr :: NodeId }
  | Proj { expr :: NodeId, projInfo :: ProjInfo }
  | Phi { preds :: [NodeId], isSE :: Bool }
  | Cond { cond :: NodeId }
  | Jump
  | NopNode
  | Exit
  deriving (Eq, Show)
  
data Value = IntVal Int32 | BoolVal Bool
  deriving (Eq, Show)

data BinOp
  = Mul | Add | Sub | Div | Mod
  | Lt | Leq | Gt | Geq
  | Eq | Neq
  | And | Or
  | BitAnd | BitOr | BitXor
  | Shl | Shr
  deriving (Eq, Show)
  
data UnOp
  = Neg | Not | BitNot
  deriving (Eq, Show)
 
 
data IRGraph = IRGraph
  { name :: String
  , successors :: IntMap IntSet
  , startBlock :: NodeId
  , endBlock :: NodeId
  , nodes :: IntMap Node
  } deriving (Eq)
  
instance Show IRGraph where
  show g = unlines $
    [ show (name g)
    , ""
    , "Successors:"
    ] ++ (map show (IntMap.toList (successors g))) ++
    [ ""
    , "startBlock: " ++ show (startBlock g)
    , "endBlock: " ++ show (endBlock g)
    , ""
    , "Nodes:"
    ] ++ (map show (IntMap.toList (nodes g)))
   
  
intVal :: Value -> Int32
intVal val = case val of
  IntVal i -> i
  BoolVal b -> if b then 1 else 0
  
newGraph :: String -> IRGraph
newGraph name = IRGraph
  { name = name
  , successors = IntMap.empty
  , startBlock = nid firstBlock
  , endBlock = nid lastBlock
  , nodes = IntMap.fromList [(nid firstBlock, firstBlock), (nid lastBlock, lastBlock)]
  }
  where
    firstBlock = Node 0 0 (Block [] "Start")
    lastBlock = Node 1 1 (Block [] "End")
  
predecessors :: Node -> [NodeId]
predecessors n =
  case nType n of
    Start -> []
    Block ps _ -> ps
    Return e _-> [e]
    ConstNode _ -> []
    BinOpNode _ l r _ -> [l, r]
    UnOpNode _ e -> [e]
    Proj e _ -> [e]
    Phi ps _ -> ps
    Cond c -> [c]
    Jump -> []
    NopNode -> []
    Exit -> []
    
addNode :: IRGraph -> Node -> IRGraph
addNode graph node =
  let nodeId = nid node
      preds = predecessors node
      newNodes = IntMap.insert nodeId node (nodes graph)
      updateSuccs = foldl (\g pred -> addSuccessor g pred nodeId) graph preds
  in updateSuccs { nodes = newNodes }

addSuccessor :: IRGraph -> NodeId -> NodeId -> IRGraph
addSuccessor graph node suc =
  graph { successors = IntMap.insertWith IntSet.union node (IntSet.singleton suc) (successors graph) }
  
  
addPredecessor :: IRGraph -> NodeId -> NodeId -> IRGraph
addPredecessor graph nodeId predId =
  case IntMap.lookup nodeId (nodes graph) of
    Nothing -> error ("Node with ID " ++ show nodeId ++ " not found")
    Just node ->
      let updatedNode = case nType node of
            Block ps n -> node { nType = Block (ps ++ [predId]) n }
            Phi ps se -> node { nType = Phi (ps ++ [predId]) se }
            _ -> error ("Can't add predecessor to node of type " ++ show (nType node))
          updatedNodes = IntMap.insert (nodeId) updatedNode (nodes graph)
      in addSuccessor (graph { nodes = updatedNodes }) predId nodeId
  
setPredecessor :: IRGraph -> Int -> NodeId -> NodeId -> IRGraph
setPredecessor graph idx nodeId predId =
  case IntMap.lookup nodeId (nodes graph) of
    Nothing -> error ("Node with ID " ++ show nodeId ++ " not found")
    Just node ->
      let updatedNode = case nType node of
            Block ps n -> node { nType = Block (take idx ps ++ [predId] ++ drop (idx - 1) ps) n }
            Phi ps se -> node { nType = Phi (take idx ps ++ [predId] ++ drop (idx - 1) ps) se }
            Return {} -> case idx of
                          0 -> node { nType = (nType node) { expr = predId } }
                          _ -> error $ "Return node has no predecessor at '" ++ show idx ++ "'"
            BinOpNode {} -> case idx of
                            0 -> node { nType = (nType node) { left = predId} }
                            1 -> node { nType = (nType node) { right = predId} }
                            _ ->  error $ "BinOp node has no predecessor at '" ++ show idx ++ "'"
            UnOpNode {} -> case idx of
                          0 -> node { nType = (nType node) { expr = predId } }
                          _ -> error $ "UnOp node has no predecessor at '" ++ show idx ++ "'"
            Cond {} -> case idx of
                          0 -> node { nType = (nType node) { cond = predId } }
                          _ -> error $ "Cond node has no predecessor at '" ++ show idx ++ "'"
            Proj {} -> case idx of
                          0 -> node { nType = (nType node) { expr = predId } }
                          _ -> error $ "Proj node has no predecessor at '" ++ show idx ++ "'"
            _ -> error ("Can't add predecessor to node of type " ++ show (nType node))
          updatedNodes = IntMap.insert (nodeId) updatedNode (nodes graph)
      in addSuccessor (graph { nodes = updatedNodes }) predId nodeId
  
  
getBlocks :: IRGraph -> [NodeId]
getBlocks = map nid . filter isBlock . IntMap.elems . nodes
  where
    isBlock node = case nType node of
      Block _ _ -> True
      _       -> False

isTerminator :: Node -> Bool
isTerminator n = case nType n of
  Cond _ -> True
  Return _ _ -> True
  Jump     -> True
  Exit      -> True
  _        -> False

getNode :: IRGraph -> NodeId -> Node
getNode g n = case IntMap.lookup n (nodes g) of
  Just node -> node
  Nothing -> error $ "Can't find Node '" ++ show n ++ "' in graph " ++ show (g)