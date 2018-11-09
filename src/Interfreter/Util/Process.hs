module Interfreter.Util.Process where

import System.IO
import System.Process

data Handles
  = Handles
    { handlesStdin   :: !Handle
    , handlesStdout  :: !Handle
    , handlesStderr  :: !Handle
    , handlesProcess :: !ProcessHandle
    }

cpHandles :: CreateProcess -> IO Handles
cpHandles cp = do
  let cpWithPipes = cp { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
  (Just i, Just o, Just e, p) <- createProcess cpWithPipes
  pure (Handles i o e p)

cleanupHandles :: Handles -> IO ()
cleanupHandles (Handles i o e p) = cleanupProcess (Just i, Just o, Just e, p)

backendProc :: FilePath -> [String] -> IO Handles
backendProc fp str = cpHandles (proc fp str)