module Interfreter.Util.Process (module Interfreter.Util.Process, module System.IO) where

import Data.List
import System.IO
import System.Process

-- | A bundle of handles to a background process. Contains
-- std{in,out,err} and the process handle.
data Handles
  = Handles
    { handlesStdin   :: !Handle
    , handlesStdout  :: !Handle
    , handlesStderr  :: !Handle
    , handlesProcess :: !ProcessHandle
    }

-- | Create a process with std{in,out,err} all set to @'CreatePipe'@.
-- Returns a bundle of handles to the process.
cpHandles :: CreateProcess -> IO Handles
cpHandles cp = do
  let cpWithPipes = cp { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
  (Just i, Just o, Just e, p) <- createProcess cpWithPipes
  pure (Handles i o e p)

-- | Terminate a process and close its handles.
cleanupHandles :: Handles -> IO ()
cleanupHandles (Handles i o e p) = cleanupProcess (Just i, Just o, Just e, p)

-- | Runs a shell command in the background and returns
-- a bundle of handles to it.
-- Forcibly routes through @sh -c@ because @cmd /c@ on Windows
-- is broken.
backendShell :: String -> IO Handles
backendShell cmd = cpHandles (proc "sh" ["-c", cmd])

-- | Read from a @'Handle'@ until the given string is encountered.
-- Returns:
--  - @Nothing@ if the handle closed before the marker was encountered
--  - @Just s@ if the marker was found. @s@ is all input read, /without/ the marker.
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

-- | Read all available input from a handle, returning the data read.
-- Waits up to 100 ms between characters.
readAllAvailable :: Handle -> IO String
readAllAvailable hdl = go ""
  where
    go acc = do
      eof <- hIsEOF hdl
      if eof
        then pure (reverse acc)
        else do
          haveInput <- hWaitForInput hdl 100
          if haveInput
            then do
              c <- hGetChar hdl
              go (c : acc)
            else pure (reverse acc)