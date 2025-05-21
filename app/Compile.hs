module Compile
  ( Job(..)
  , compile
  ) where

import Compile.Asm (codeGen)
import Compile.Parser (parseAST)
import Compile.Semantic (semanticAnalysis)
import Compile.X86 (printX86)
import Compile.IR (irTranslate)
import Error (L1ExceptT)

import Control.Monad.IO.Class

data Job = Job
  { src :: FilePath
  , out :: FilePath
  } deriving (Show)

compile :: Job -> L1ExceptT ()
compile job = do
  ast <- parseAST $ src job
  semanticAnalysis ast
  let ir = irTranslate "main" ast
  let code = codeGen ir
  liftIO $ writeFile (out job) (printX86 code)
  return ()
