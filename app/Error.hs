{-# LANGUAGE FlexibleInstances #-}
module Error
  ( L1ExceptT
  , MonadCompilerFail
  , generalFail
  , parserFail
  , semanticFail
  , dieWithError
  ) where

import           Control.Monad.Except (ExceptT, throwError)
import qualified System.Exit as Exit
import           System.IO (hPutStrLn, stderr)
import           Control.Monad.State.Strict (lift, StateT)

-- Predefined exit codes signaling compiler status
parserErrorCode :: Int
parserErrorCode = 42

semanticErrorCode :: Int
semanticErrorCode = 7

-- Error message and exit code
data L1Error
  = Error String Int
  | ParserError String
  | SemanticError String
  deriving (Show)

type L1ExceptT = ExceptT L1Error IO

class Monad m => MonadCompilerFail m where
  generalFail :: String -> Int -> m a
  parserFail :: String -> m a
  semanticFail :: String -> m a

instance MonadCompilerFail L1ExceptT where
  -- Convenienve functions to throw exceptions
  generalFail msg code = throwError $ Error msg code
  parserFail = throwError . ParserError
  semanticFail = throwError . SemanticError

instance MonadCompilerFail m => MonadCompilerFail (StateT s m) where
  generalFail msg code = lift (generalFail msg code)
  parserFail = lift . parserFail
  semanticFail = lift . semanticFail

-- Exit with an error message and a return code
dieWithError :: L1Error -> IO ()
dieWithError (Error msg code) = do
  hPutStrLn stderr msg
  Exit.exitWith $ Exit.ExitFailure code
dieWithError (ParserError msg) = dieWithError (Error msg parserErrorCode)
dieWithError (SemanticError msg) = dieWithError (Error msg semanticErrorCode)
