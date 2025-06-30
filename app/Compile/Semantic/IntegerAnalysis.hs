module Compile.Semantic.IntegerAnalysis
  ( checkIntegers
  ) where
  
import Compile.Semantic.Util
import Compile.Frontend.AST
import Compile.Semantic.Traverse
import Compile.Semantic.TraversalStates
import Compile.Frontend.Parser (parseNumber)

checkIntegers :: Handler NoState
checkIntegers = defaultHandler
  { hIntExpr = checkInt
  }
  
checkInt :: String -> SourcePos -> NoState ()
checkInt s pos = case parseNumber s of
  Left e -> semanticFail' $ "Invalid integer at " ++ posPretty pos ++ ": " ++ posPretty pos
  Right _ -> pure ()