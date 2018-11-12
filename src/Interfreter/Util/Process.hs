module Interfreter.Util.Process (module Interfreter.Util.Process, module System.IO) where

import Control.Exception
import Data.List
import System.IO
import System.IO.Error

import System.Process.Typed

-- | Set all three of a process config's standard in/out/err to @'createPipe'@.
setCreatePipes :: ProcessConfig i o e -> ProcessConfig Handle Handle Handle
setCreatePipes = setStdin createPipe . setStdout createPipe . setStderr createPipe

-- | Read from a handle until EOF or the given string
-- is encountered. Returns all input read, not including the
-- given string.
readTill :: Handle -> String -> IO String
readTill hdl sem = reverse <$> go ""
  where
    rsem = reverse sem
    slen = length sem
    go !acc =
      if rsem `isPrefixOf` acc
        then pure (drop slen acc)
        else handle (\(_ :: IOException) -> pure acc) $ do
          c <- hGetChar hdl
          go (c : acc)