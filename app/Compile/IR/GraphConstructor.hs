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
  , breakTarget :: [NodeId]
  , continueTarget :: [NodeId]
  }

emptyState :: GraphState
emptyState = GraphState
  { graph = graph
  , currentDef = Map.empty
  , incompletePhis = IntMap.empty
  , currentSideEffect = IntMap.empty
  , incompleteSEPhis = IntMap.empty
  , sealedBlocks = IntSet.singleton (startBlock graph)
  , currentBlock = startBlock graph
  , nextNodeId = IntMap.size (nodes graph)
  , breakTarget = []
  , continueTarget = []
  }
  where
    graph = newGraph "main"
  

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

newExit :: GraphConstructor Node
newExit = newNode Exit

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

newPhi :: NodeId -> [NodeId] -> Bool -> GraphConstructor Node
newPhi blk ps se = newNode_ blk (Phi ps se)

newBlock :: [NodeId] -> GraphConstructor Node
newBlock ps = do
  n <- freshId
  let node = Node n n (Block ps)
  registerNode node
  return node
  
newBranch :: NodeId -> GraphConstructor Node
newBranch cond = newNode (Cond cond)

newIf :: NodeId -> GraphConstructor Node
newIf cond = newNode (Cond cond)

newJump :: GraphConstructor Node
newJump = newNode Jump

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
            phiId <- nid <$> newPhi blk [] False
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
              [p] -> do
                pBlk <- block <$> node p
                readVar name pBlk
              _ -> do
                phiId <- nid <$> newPhi blk [] False
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
        Phi ps se -> phi { nType = Phi (ps ++ [n]) se }
        _ -> error "appendPhiOperand: not a Phi node"
  modify $ \s -> s { graph = addSuccessor ((graph s) { nodes = IntMap.insert phiId updated (nodes (graph s)) }) n phiId }

addPhiOperands :: Name -> NodeId -> GraphConstructor NodeId
addPhiOperands name phiId = do
  blk <- block <$> node phiId
  preds <- predecessors <$> node blk
  forM_ preds $ \p -> do
    pBlk <- block <$> node p
    v <- readVar name pBlk
    appendPhiOperand phiId v
  tryRemoveTrivialPhi phiId
  
addSEPhiOperands :: NodeId -> GraphConstructor NodeId
addSEPhiOperands phiId = do
  blk <- block <$> node phiId
  preds <- predecessors <$> (node blk)
  forM_ preds $ \p -> do
    pBlk <- block <$> node p
    v <- readSideEffect pBlk
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
             phiId <- nid <$> newPhi blk [] True
             modify $ \s -> s { incompleteSEPhis = IntMap.insert blk phiId (incompleteSEPhis s) }
             return phiId
           else do
             preds <- predecessors <$> node blk
             case preds of
               [p] -> do
                 pBlk <- block <$> node p
                 readSideEffect pBlk
               _   -> do
                 phiId <- nid <$> newPhi blk [] True
                 writeSideEffect blk phiId
                 addSEPhiOperands phiId
  writeSideEffect blk val
  return val
  
addBreakTarget :: NodeId -> GraphConstructor ()
addBreakTarget t = modify $ \s -> s { breakTarget = t : (breakTarget s)}

removeBreakTarget :: GraphConstructor ()
removeBreakTarget = modify $ \s -> s { breakTarget = tail (breakTarget s)}

addContinueTarget :: NodeId -> GraphConstructor ()
addContinueTarget t = modify $ \s -> s { continueTarget = t : (continueTarget s)}

removeContinueTarget :: GraphConstructor ()
removeContinueTarget = modify $ \s -> s { continueTarget = tail (continueTarget s)}
