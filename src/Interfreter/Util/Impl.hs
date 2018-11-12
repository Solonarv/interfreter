module Interfreter.Util.Impl where

import Interfreter.Util.Process

import System.Process.Typed

import System.IO


-- | A reasonable default definition for @'Interfreter.Types.freeInterpreter'@,
-- suitable for interpreters that just consist of a background process
-- from @typed-process@.
defaultFreeInterpreter :: (ip -> Process i o e) -> ip -> IO ()
defaultFreeInterpreter getHdl = stopProcess . getHdl


{-

-- | A reasonable default definition for @'Interfreter.Types.runInterpreterOn'@.
-- Suitable for interpreters that are a bundle of pipes connected to a
-- REPL process running in the background.
defaultRunInterpreterOn :: (i -> Handles) -> i -> String -> String -> IO (Either String String)
defaultRunInterpreterOn fetchHdl i _ code = do
  let handles = fetchHdl i
  hPutStrLn (handlesStdin handles) code
  err <- readAllAvailable (handlesStderr handles)
  out <- readAllAvailable (handlesStdout handles)
  if (null err)
    then pure (Right out)
    else pure (Left err)

-}