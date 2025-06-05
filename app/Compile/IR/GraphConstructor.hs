module Compile.IR.GraphConstructor where

import           Compile.IR.IRGraph

import           Control.Monad.State
import           Control.Monad (forM_, void)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Control.Arrow ((>>>))
import           Data.Maybe (fromJust)


data GraphState = GraphState
  { graph :: IRGraph
  , currentDef :: Map Name (IntMap NodeId)
  , incompletePhis :: IntMap (Map Name NodeId)
  , currentSideEffect :: IntMap NodeId
  , incompleteSEPhis :: IntMap NodeId
  , sealedBlocks :: IntSet
  , currentBlock :: NodeId
  , nextNodeId :: NodeId
  , breakTarget :: Maybe NodeId
  , continueTarget :: Maybe NodeId
  }

emptyState :: GraphState
emptyState = GraphState
  { graph = startGraph
  , currentDef = Map.empty
  , incompletePhis = IntMap.empty
  , currentSideEffect = IntMap.singleton 0 2
  , incompleteSEPhis = IntMap.empty
  , sealedBlocks = IntSet.singleton 0
  , currentBlock = 0
  , nextNodeId = 3
  , breakTarget = Nothing
  , continueTarget = Nothing
  }

type GraphConstructor a = State GraphState a

freshId :: GraphConstructor NodeId
freshId = do
  n <- gets nextNodeId
  modify $ \s -> s { nextNodeId = n + 1 }
  return n

registerNode :: Node -> GraphConstructor ()
registerNode n = do
  irGraph <- gets graph
  modify $ \s -> s { graph = addNode (graph s) n }
  

newNode :: NodeType -> GraphConstructor Node
newNode t = do
  blk <- gets currentBlock
  newNode_ blk t
  
newNode_ :: NodeId -> NodeType -> GraphConstructor Node
newNode_ blk t = do
  n <- freshId
  let node = (Node n blk t)
  registerNode node
  return node

newStart :: GraphConstructor Node
newStart = newNode Start

newBinOp :: BinOp -> NodeId -> NodeId -> Maybe NodeId -> GraphConstructor Node
newBinOp op l r se = newNode (BinOpNode op l r se)

newUnOp :: UnOp -> NodeId -> GraphConstructor Node
newUnOp op e = newNode (UnOpNode op e)

newReturn :: NodeId -> Maybe NodeId -> GraphConstructor Node
newReturn e se = newNode (Return e se)

newConst :: Value -> GraphConstructor Node
newConst i = newNode (ConstNode i)
  
newProj :: NodeId -> ProjInfo -> GraphConstructor Node
newProj n info = newNode (Proj n info)

newPhi :: NodeId -> [NodeId] -> GraphConstructor Node
newPhi blk ps = newNode_ blk (Phi ps)

newBlock :: [NodeId] -> GraphConstructor Node
newBlock ps = do
  n <- freshId
  let node = Node n n (Block ps)
  registerNode node
  return node
  
newBranch :: NodeId -> NodeId -> NodeId -> GraphConstructor Node
newBranch cond trueBlk falseBlk = newNode (Branch cond trueBlk falseBlk)

newJump :: NodeId -> GraphConstructor ()
newJump target = do
  blk <- gets currentBlock
  modify $ \s -> s { graph = addPredecessor (graph s) target blk }


writeVar :: Name -> NodeId -> NodeId -> GraphConstructor ()
writeVar name blk val = modify $ \s -> s {
  currentDef = Map.insertWith
                (\_ old -> IntMap.insert blk val old)
                name
                (IntMap.singleton blk val)
                (currentDef s)
  }
  
  
readVar :: Name -> NodeId -> GraphConstructor NodeId
readVar name blk = do
  defs <- gets currentDef
  case IntMap.lookup blk (Map.findWithDefault IntMap.empty name defs) of
    Just n -> pure n
    Nothing -> readVarRecursive name blk

readVarRecursive :: Name -> NodeId -> GraphConstructor NodeId
readVarRecursive name blk = do
  sealed <- gets sealedBlocks
  val <- if not (IntSet.member blk sealed)
          then do
            phiId <- nid <$> newPhi blk []
            modify $ \s -> s
              { incompletePhis = IntMap.insertWith
                                  Map.union
                                  blk
                                  (Map.singleton name phiId)
                                  (incompletePhis s)
              }
            return phiId
          else do
            preds <- predecessors <$> node blk
            case preds of
              [p] -> readVar name p
              _ -> do
                phiId <- nid <$> newPhi blk []
                writeVar name blk phiId
                addPhiOperands name phiId
  writeVar name blk val
  return val
  

node :: NodeId -> GraphConstructor Node
node n = do
  ns <- gets (nodes . graph)
  return (ns IntMap.! n)
  
appendPhiOperand :: NodeId -> NodeId -> GraphConstructor ()
appendPhiOperand phiId n = do
  phi <- node phiId
  let updated = case nType phi of
        Phi ps -> phi { nType = Phi (ps ++ [n]) }
        _ -> error "appendPhiOperand: not a Phi node"
  modify $ \s -> s { graph = addSuccessor ((graph s) { nodes = IntMap.insert phiId updated (nodes (graph s)) }) n phiId }

addPhiOperands :: Name -> NodeId -> GraphConstructor NodeId
addPhiOperands name phiId = do
  blk <- block <$> node phiId
  preds <- predecessors <$> node blk
  forM_ preds $ \p -> do
    v <- readVar name p
    appendPhiOperand phiId v
  tryRemoveTrivialPhi phiId
  
addSEPhiOperands :: NodeId -> GraphConstructor NodeId
addSEPhiOperands phiId = do
  blk <- block <$> node phiId
  preds <- predecessors <$> (node blk)
  forM_ preds $ \p -> do
    v <- readSideEffect p
    appendPhiOperand phiId v
  tryRemoveTrivialPhi phiId
  
  
tryRemoveTrivialPhi :: NodeId -> GraphConstructor NodeId
tryRemoveTrivialPhi n = pure n

setCurrentBlock :: NodeId -> GraphConstructor ()
setCurrentBlock b = modify $ \s -> s { currentBlock = b }


sealBlock :: NodeId -> GraphConstructor ()
sealBlock blk = do
  -- finish all incomplete Phis
  mip <- gets (IntMap.findWithDefault Map.empty blk . incompletePhis)
  forM_ (Map.toList mip) $ \(name, phiId) -> void (addPhiOperands name phiId)
  -- side-effect Phis
  msep <- gets (IntMap.lookup blk . incompleteSEPhis)
  forM_ msep $ \phiId -> void (addSEPhiOperands phiId)
  -- mark sealed
  modify $ \s -> s { sealedBlocks = IntSet.insert blk (sealedBlocks s) }

writeCurrentSideEffect :: NodeId -> GraphConstructor ()
writeCurrentSideEffect n = do
  blk <- gets currentBlock
  writeSideEffect blk n

writeSideEffect :: NodeId -> NodeId -> GraphConstructor ()
writeSideEffect blk n = modify $ \s -> s { currentSideEffect = IntMap.insert blk n (currentSideEffect s) }

readCurrentSideEffect :: GraphConstructor NodeId
readCurrentSideEffect = gets currentBlock >>= readSideEffect

readSideEffect :: NodeId -> GraphConstructor NodeId
readSideEffect blk = do
  m <- gets (IntMap.lookup blk . currentSideEffect)
  case m of
    Just n  -> return n
    Nothing -> readSideEffectRecursive blk

readSideEffectRecursive :: NodeId -> GraphConstructor NodeId
readSideEffectRecursive blk = do
  sealed <- gets sealedBlocks
  val <- if not (IntSet.member blk sealed)
           then do
             phiId <- nid <$> newPhi blk []
             modify $ \s -> s { incompleteSEPhis = IntMap.insert blk phiId (incompleteSEPhis s) }
             return phiId
           else do
             preds <- predecessors <$> node blk
             case preds of
               [p] -> readSideEffect p
               _   -> do
                 phiId <- nid <$> newPhi blk []
                 writeSideEffect blk phiId
                 addSEPhiOperands phiId
  writeSideEffect blk val
  return val

insertMergePhis :: NodeId -> NodeId -> NodeId -> GraphConstructor ()
insertMergePhis firstBlk secondBlk mergeBlk = do
  blk <- gets currentBlock
  defs <- gets currentDef
  let bothDefined = Map.filter (\imap -> IntMap.member firstBlk imap && IntMap.member secondBlk imap) defs
  forM_ (Map.toList bothDefined) $ \(var, imap) -> do
    let firstVal = imap IntMap.! firstBlk
        secondVal = imap IntMap.! secondBlk
    phi <- newPhi blk [firstVal, secondVal]
    let updated = IntMap.insert mergeBlk (nid phi) imap
    modify $ \s -> s { currentDef = Map.insert var updated defs }
    