module Interfreter.Types where

import Data.Kind (Type)

-- | The class of interpreter contexts. An interpreter context
-- represents a connection to some REPL; this class provides
-- uniform methods for creating, freeing, and using them.
--
-- Note: the @"Monoid"@ instance for @cfg@ should be right-biased.
class (Monoid cfg) => Interpreter cfg i | i -> cfg, cfg -> i where
  -- | The list of languages this interpreter can handle.
  -- This should be a list of strings of the form @["hs", "haskell"]@ for
  -- e.g. a Haskell interpreter.
  interpreterLangs :: proxy i -> [String]

  -- | Get information about an interpreter. This should return
  -- a short string describing e.g. compiler/interpreter/language
  -- version.
  interpreterInfo :: i -> String

  -- | Create an interpreter context. Commonly, this method
  -- might create a background process according to the
  -- configuration, and store the process' handle in @i@.
  createInterpreter :: cfg -> IO i
  
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