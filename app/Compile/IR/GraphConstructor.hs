module Compile.IR.GraphConstructor where

import           Compile.IR.IRGraph
import           Compile.IR.Optimize.Optimizer (optimizeNode)

import           Control.Monad.State.Strict
import           Control.Monad (forM_, void, unless)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
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
instance Show GraphState where
  show g = unlines $
    [ "irGraph: " ++ show (graph g)
    , "currentDef: " ++ show (currentDef g)
    , "incompletePhis: " ++ show (incompletePhis g)
    , "currentSideEffect: " ++ show (currentSideEffect g)
    , "incompleteSEPhis: " ++ show (incompleteSEPhis g)
    , "sealedBlocks: " ++ show (sealedBlocks g)
    , "currentBlock: " ++ show (currentBlock g)
    , "nextNodeId: " ++ show (nextNodeId g)]

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
  ir <- gets graph
  let node = optimizeNode ir (Node n blk t)
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
newConst i = do
  --blk <- gets (startBlock . graph)
  newNode (ConstNode i)
  
newProj :: NodeId -> ProjInfo -> GraphConstructor Node
newProj n info = newNode (Proj n info)

newPhi :: NodeId -> [NodeId] -> Bool -> GraphConstructor Node
newPhi blk ps se = newNode_ blk (Phi ps se)

newBlock :: [NodeId] -> String -> GraphConstructor Node
newBlock ps name = do
  n <- freshId
  let node = Node n n (Block ps name)
  registerNode node
  return node
  
newBranch :: NodeId -> GraphConstructor Node
newBranch cond = newNode (Cond cond)

newIf :: NodeId -> GraphConstructor Node
newIf cond = newNode (Cond cond)

newJump :: GraphConstructor Node
newJump = newNode Jump

newNopNode :: NodeId -> GraphConstructor Node
newNopNode blk = newNode_ blk NopNode

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
  ir <- gets graph
  let ns = nodes ir
  case IntMap.lookup n ns of
    Just x -> pure x
    Nothing -> error $ "Couldn't find node '" ++ show n ++ "' in irGraph: " ++ show ir
  
appendPhiOperand :: NodeId -> NodeId -> GraphConstructor ()
appendPhiOperand phiId n = do
  phi <- node phiId
  let updated = case nType phi of
        Phi ps se -> phi { nType = Phi (ps ++ [n]) se }
        _ -> error "appendPhiOperand: not a Phi node"
  modify $ \s -> s { graph = addSuccessor ((graph s) { nodes = IntMap.insert phiId updated (nodes (graph s)) }) n phiId }

addPhiOperands :: Name -> NodeId -> GraphConstructor NodeId
addPhiOperands name phiId = do
  ir <- gets graph
  blk <- block <$> node phiId
  preds <- predecessors <$> node blk
  forM_ preds $ \p -> do
    pBlk <- block <$> node p
    v <- readVar name pBlk
    n <- node v
    case nType n of
      NopNode {} -> pure ()
      _ -> appendPhiOperand phiId v
  tryRemoveTrivialPhi phiId
  
addSEPhiOperands :: NodeId -> GraphConstructor NodeId
addSEPhiOperands phiId = do
  ir <- gets graph
  blk <- block <$> node phiId
  preds <- predecessors <$> (node blk)
  forM_ preds $ \p -> do
    pBlk <- block <$> node p
    v <- readSideEffect pBlk
    n <- node v
    case nType n of
      NopNode {} -> pure ()
      _ -> appendPhiOperand phiId v
  tryRemoveTrivialPhi phiId
  
  
tryRemoveTrivialPhi :: NodeId -> GraphConstructor NodeId
tryRemoveTrivialPhi n = do
  pure n
  {-
  ir <- gets graph
  sBlocks <- gets sealedBlocks
  let phi = getNode ir n
      preds = IntSet.toList $ IntSet.delete n (IntSet.fromList (predecessors phi))
  case preds of
    [] -> nid <$> newNopNode (block phi)
    [t] -> do
      let succs = IntSet.toList $ IntMap.findWithDefault IntSet.empty n (successors ir)
      forM_
        succs
        (\s -> do
          let ps = predecessors (getNode ir s)
          forM_ (filter ((== n) . snd) (zip [0..] ps)) (\(i, _) -> do
            modify $ \st -> st { graph = setPredecessor (graph st) i s t }
            case nType (getNode ir s) of
              Phi {} -> if block (getNode ir s) `IntSet.member` sBlocks
                          then tryRemoveTrivialPhi s
                          else pure t
              _ -> pure t
            )
        )
      return t
    _ -> return n
   -}

setCurrentBlock :: NodeId -> GraphConstructor ()
setCurrentBlock b = modify $ \s -> s { currentBlock = b }


sealBlock :: NodeId -> GraphConstructor ()
sealBlock blk = do
  state <- get
  unless (blk `IntSet.member` (sealedBlocks state)) $ do
      forM_ (Map.toList $ IntMap.findWithDefault Map.empty blk (incompletePhis state)) (\(v, p) -> do
        n <- addPhiOperands v p
        let vDefs = (Map.!) (currentDef state) v
        if IntMap.lookup blk vDefs == Just p
          then modify $ \s -> s { currentDef = Map.insert v (IntMap.insert n blk vDefs) (currentDef s) }
          else pure ()
        )
      modify $ \s -> s { incompletePhis = IntMap.delete blk (incompletePhis s) }
      state <- get
      case IntMap.lookup blk (incompleteSEPhis state) of
        Just phi -> addSEPhiOperands phi
        Nothing -> pure 1
      modify $ \s -> s { incompleteSEPhis = IntMap.delete blk (incompleteSEPhis s) }
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
