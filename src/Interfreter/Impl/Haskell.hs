module Interfreter.Impl.Haskell (Haskell(..)) where

import Control.Applicative
import Data.Semigroup

import System.Process.Typed

import Interfreter.Types
import Interfreter.Util.Process
import Interfreter.Util.Impl

data Haskell
  = Haskell
    { cmd     :: !FilePath
    , infoStr :: !String
    , procCfg :: !(ProcessConfig Handle Handle Handle)
    , theGhci :: !(Process Handle Handle Handle)
    }

instance Interpreter Haskell where
  interpreterLangs _ = ["hs", "haskell"]
  interpreterInfo = infoStr

  createInterpreter cmd = do
    let procCfg = setCreatePipes $ shell cmd
    theGhci <- startProcess procCfg
    let stdout = getStdout theGhci
        stdin  = getStdin theGhci
    _            <- readTill stdout "GHCi, version "
    version <- readTill stdout ":"
    let infoStr = "ghci --version " <> version
    hPutStrLn stdin ":set prompt \"\""
    hPutStrLn stdin "putStrLn \"0e587hw047gh0s87zr0gtwgn-er8nepr89y59\""
    _ <- readTill stdout "0e587hw047gh0s87zr0gtwgn-er8nepr89y59"
    pure Haskell{cmd, infoStr, procCfg, theGhci}
  
  freeInterpreter = defaultFreeInterpreter theGhci

  runInterpreterOn = error "TODO"