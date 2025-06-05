module Compile
  ( Job(..)
  , compile
  ) where

import Compile.Backend.Asm (codeGen)
import Compile.Frontend.Parser (parseAST)
import Compile.Semantic.Semantic (semanticAnalysis)
import Compile.Backend.X86.X86 (printX86)
import Compile.IR.SSA (irTranslate)
import Compile.IR.ControlFlow (buildCFG)
import Error (L1ExceptT)

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
  let cfg = buildCFG ir
      code = codeGen cfg
  liftIO $ writeFile (out job) (printX86 code)
  return ()
