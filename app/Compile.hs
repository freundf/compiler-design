module Compile
  ( Job(..)
  , compile
  ) where

import Compile.Backend.Asm (codeGen)
import Compile.Frontend.Parser (parseAST)
import Compile.Semantic.Semantic (semanticAnalysis)
import Compile.Backend.X86.X86 (printX86)
import Compile.Backend.Schedule (schedule)
import Compile.IR.SSA (irTranslate)
import Compile.IR.RegAlloc
import Compile.IR.IRGraph
import Error (L1ExceptT)


import qualified Data.IntMap as IntMap
import Control.Monad.IO.Class

data Job = Job
  { src :: FilePath
  , out :: FilePath
  } deriving (Show)

compile :: Job -> L1ExceptT ()
compile job = do
  ast <- parseAST $ src job
  liftIO $ print ast
  semanticAnalysis ast
  let ir = irTranslate ast
  liftIO $ print ir
  let order = schedule ir
      regMap = naiveStrategy (fst . IntMap.findMax $ nodes ir)
  let code = codeGen regMap order ir
  liftIO $ writeFile (out job) (printX86 code)
  return ()
