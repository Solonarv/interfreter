{-# LANGUAGE AllowAmbiguousTypes #-}
module Interfreter.Types where

import Data.Kind
import Data.Proxy

import Interfreter.Util.TypeLevel

-- | The class of interpreter contexts. An interpreter context
-- represents a connection to some REPL; this class provides
-- uniform methods for creating, freeing, and using them.
class (KnownSymbolList (Langs i)) => Interpreter i where

  -- | The type of configuration this interpreter needs.
  -- This might include e.g. a path to the REPL binary,
  -- or a set of packages to bring into scope.
  type Config i :: Type

  -- | The list of languages this interpreter can handle.
  -- This should be a list of strings of the form @["hs", "haskell"]@ for
  -- e.g. a Haskell interpreter.
  type Langs i :: [Symbol]

  -- | Get information about an interpreter. This should return
  -- a short string describing e.g. compiler/interpreter/language
  -- version.
  interpreterInfo :: i -> String

  -- | Create an interpreter context. Commonly, this method
  -- might create a background process according to the
  -- configuration, and store the process' handle in @i@.
  createInterpreter :: Config i -> IO i
  
  -- | Free any resources held by an interpreter context.
  -- Once freed, the interpreter context will no longer be usable.
  -- This method might terminate the background process created by
  -- @'createInterpreter'@ and drop the handle.
  freeInterpreter :: i -> IO ()

  -- | Pass a string of code to the interpreter, and return its output.
  runInterpreterOn :: i      -- ^ interpreter state
                   -> String -- ^ language identifier
                   -> String -- ^ code fragment
                   -> IO (Either String String)

interpreterLangs' :: forall i. Interpreter i => [String]
interpreterLangs' = symbolListVal (Proxy @(Langs i))