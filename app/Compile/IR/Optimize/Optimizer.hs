module Compile.IR.Optimize.Optimizer where

import Compile.IR.Optimize.ConstantFolding (constantFold)
import Compile.IR.IRGraph

import Control.Monad (foldM)

optimizers :: [IRGraph -> Node -> Node]
optimizers = [constantFold]

optimizeNode :: IRGraph -> Node -> Node
optimizeNode ir n = foldl (\n o -> o ir n) n optimizers