module Interfreter.Util.Process where

import Data.List
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

backendShell :: String -> IO Handles
backendShell cmd = cpHandles (shell cmd)

readTill :: Handle -> String -> IO (Maybe String)
readTill hdl sem = go ""
  where
    rsem = reverse sem
    slen = length rsem
    go acc =
      if rsem `isPrefixOf` acc
        then pure (Just (reverse $ drop slen acc))
        else do
          eof <- hIsEOF hdl
          if eof
            then pure Nothing
            else do
              c <- hGetChar hdl
              go (c : acc)