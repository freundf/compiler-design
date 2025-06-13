module Compile.Semantic.NameAnalysis
  ( resolveNames
  ) where
  
  
import Compile.Semantic.Util
import Compile.Semantic.Traverse
import Compile.Frontend.AST

import Control.Monad (void)


resolveNames :: Handler Sem
resolveNames = defaultHandler
  { hDecl = resolveDecl
  , hInit = resolveInit
  , hAsgn = resolveAsgn
  , hIdent = resolveIdent
  }

resolveDecl :: Type -> String -> SourcePos -> Semantic ()
resolveDecl ty name pos = insertVar name (VarInfo ty False) pos

resolveInit :: Type -> String -> Expr -> SourcePos -> Semantic ()
resolveInit ty name expr pos = insertVar name (VarInfo ty False) pos

resolveAsgn :: String -> AsgnOp -> Expr -> SourcePos -> Semantic ()
resolveAsgn name _ expr pos = void $ lookupVar name pos

resolveIdent :: String -> SourcePos -> Semantic ()
resolveIdent name pos = void $ lookupVar name pos