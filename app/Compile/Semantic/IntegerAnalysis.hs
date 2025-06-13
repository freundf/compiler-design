module Compile.Semantic.IntegerAnalysis
  ( checkIntegers
  ) where
  
import Compile.Semantic.Util
import Compile.Frontend.AST
import Compile.Semantic.Traverse
import Compile.Frontend.Parser (parseNumber)

checkIntegers :: Handler Sem
checkIntegers = defaultHandler
  { hIntExpr = checkInt
  }
  
checkInt :: String -> SourcePos -> Semantic ()
checkInt s pos = case parseNumber s of
  Left e -> semanticFail' $ "Invalid integer at " ++ posPretty pos ++ ": " ++ posPretty pos
  Right _ -> pure ()