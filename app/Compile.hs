module Compile
  ( Job(..)
  , compile
  ) where

import Compile.Asm (codeGen)
import Compile.Parser (parseAST)
import Compile.Semantic (semanticAnalysis)
import Compile.X86 (printX86)
import Error (L1ExceptT)

import Control.Monad.IO.Class (liftIO)
import System.Process (callProcess)
import System.FilePath ((-<.>))

data Job = Job
  { src :: FilePath
  , out :: FilePath
  } deriving (Show)

compile :: Job -> L1ExceptT ()
compile job = do
  ast <- parseAST $ src job
  semanticAnalysis ast
  let code = codeGen ast
  liftIO $ do
    let out_s = out job -<.> "s"
    writeFile (out_s) (printX86 code)
    callProcess "gcc" [out_s, "-o", out job]
  return ()
