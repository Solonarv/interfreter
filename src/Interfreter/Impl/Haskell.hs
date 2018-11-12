module Interfreter.Impl.Haskell (Haskell(..)) where

import Control.Applicative
import Data.Semigroup

import Interfreter.Types
import Interfreter.Util.Process
import Interfreter.Util.Impl

data Haskell
  = Haskell
    { cmd     :: !FilePath
    , infoStr :: !String
    , handles :: !Handles
    }

instance Interpreter Haskell where
  interpreterLangs _ = ["hs", "haskell"]
  interpreterInfo = infoStr

  createInterpreter cmd = do
    handles@Handles{handlesStdout, handlesStdin} <- backendShell cmd
    _            <- readTill handlesStdout "GHCi, version "
    Just version <- readTill handlesStdout ":"             
    let infoStr = "ghci --version " <> version
    hPutStrLn handlesStdin ":set prompt \"\""
    hPutStrLn handlesStdin "putStrLn \"0e587hw047gh0s87zr0gtwgn-er8nepr89y59\""
    _ <- readTill handlesStdout "0e587hw047gh0s87zr0gtwgn-er8nepr89y59"
    pure Haskell{cmd, infoStr, handles}
  
  freeInterpreter = cleanupHandles . handles

  runInterpreterOn = defaultRunInterpreterOn handles