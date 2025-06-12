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
  , addSuccessor
  , addNode
  , isTerminator
  , getBlocks
  , getNode
  ) where

  
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.List (intercalate)


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
  | Block { preds :: [NodeId] }
  | Return { expr :: NodeId, sideEffect :: Maybe NodeId }
  | ConstNode { value :: Value }
  | BinOpNode { binOp :: BinOp, left :: NodeId, right :: NodeId, sideEffect :: Maybe NodeId }
  | UnOpNode { unOp :: UnOp, expr :: NodeId }
  | Proj { expr :: NodeId, projInfo :: ProjInfo }
  | Phi { preds :: [NodeId], isSE :: Bool }
  | Cond { cond :: NodeId }
  | Jump
  | Exit
  deriving (Eq, Show)
  
data Value = IntVal Int | BoolVal Bool
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
  } deriving (Eq, Show)
  

  
newGraph :: String -> IRGraph
newGraph name = IRGraph
  { name = name
  , successors = IntMap.empty
  , startBlock = nid firstBlock
  , endBlock = nid lastBlock
  , nodes = IntMap.fromList [(nid firstBlock, firstBlock), (nid lastBlock, lastBlock)]
  }
  where
    firstBlock = Node 0 0 (Block [])
    lastBlock = Node 1 1 (Block [])
  
predecessors :: Node -> [NodeId]
predecessors n =
  case nType n of
    Start -> []
    Block ps -> ps
    Return e _-> [e]
    ConstNode _ -> []
    BinOpNode _ l r _ -> [l, r]
    UnOpNode _ e -> [e]
    Proj e _ -> [e]
    Phi ps _ -> ps
    Cond c -> [c]
    Jump -> []
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
            Block ps -> node { nType = Block (ps ++ [predId]) }
            Phi ps se -> node { nType = Phi (ps ++ [predId]) se }
            _ -> error ("Can't add predecessor to node of type " ++ show (nType node))
          updatedNodes = IntMap.insert (nodeId) updatedNode (nodes graph)
      in addSuccessor (graph { nodes = updatedNodes }) predId nodeId
  
getBlocks :: IRGraph -> [NodeId]
getBlocks = map nid . filter isBlock . IntMap.elems . nodes
  where
    isBlock node = case nType node of
      Block _ -> True
      _       -> False

isTerminator :: Node -> Bool
isTerminator n = case nType n of
  Cond _ -> True
  Return _ _ -> True
  Jump     -> True
  Exit      -> True
  _        -> False

getNode :: IRGraph -> NodeId -> Node
getNode g n = (nodes g) IntMap.! n