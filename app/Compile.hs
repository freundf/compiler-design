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
import Error (L1ExceptT)

import Control.Monad.IO.Class
import System.Process (callProcess)
import System.FilePath (replaceExtension)

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
      code = codeGen ir
  liftIO $ assemble (out job) (printX86 code)
  return ()


assemble :: FilePath -> String -> IO ()
assemble file code = do
    let asmFile = replaceExtension file "s"
        objFile = replaceExtension file "o"
    writeFile asmFile code
    callProcess "gcc" ["-c", asmFile, "-o", objFile]
    callProcess "gcc" [objFile, "stdlib/stdlib.o", "-o", file]
