module Compile.Semantic.NameAnalysis
  ( resolveNames
  ) where
  
  
import Compile.Semantic.Util
import Compile.Semantic.Traverse
import Compile.Frontend.AST

import Control.Monad (void)
import Control.Monad.State.Strict (modify)


resolveNames :: Handler Sem
resolveNames = defaultHandler
  { hFuncEnter = prepareCtx
  , hDecl = resolveDecl
  , hInit = resolveInit
  , hAsgn = resolveAsgn
  , hIdent = resolveIdent
  }

prepareCtx :: Function -> Semantic ()
prepareCtx _ = modify $ \s -> s { scopes = [], oldScopes = [], loopDepth = 0, returnType = TAny, recordedTypes = [] }

resolveDecl :: Type -> String -> SourcePos -> Semantic ()
resolveDecl ty name pos = insertVar name (VarInfo ty False) pos

resolveInit :: Type -> String -> Expr -> SourcePos -> Semantic ()
resolveInit ty name expr pos = insertVar name (VarInfo ty True) pos

resolveAsgn :: String -> AsgnOp -> Expr -> SourcePos -> Semantic ()
resolveAsgn name _ expr pos = void $ lookupVar name pos

resolveIdent :: String -> SourcePos -> Semantic ()
resolveIdent name pos = void $ lookupVar name pos