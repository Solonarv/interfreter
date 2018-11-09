module Interfreter.Impl.Haskell where

import Control.Applicative
import Data.Semigroup

import Interfreter.Types
import Interfreter.Util.Process

data Haskell
  = Haskell
    { cfg     :: !HaskellCfg
    , infoStr :: !String
    , handles :: !Handles
    }

data HaskellCfg
  = StackGhci
    { hs_packages :: ![String]
    , hs_resolver :: !(Maybe String)
    , hs_args     :: ![String]
    }
  | CustomGhci
    { hs_exe  :: !FilePath
    , hs_args :: ![String]
    }

instance Semigroup HaskellCfg where
  frst@StackGhci{} <> scnd@StackGhci{}
    = StackGhci
      { hs_packages = hs_packages frst <> hs_packages scnd
      , hs_resolver = hs_resolver scnd <|> hs_resolver frst -- right bias!
      , hs_args     = hs_args frst <> hs_args scnd  
      }
  _ <> x@StackGhci{} = x
  _ <> x@CustomGhci{} = x
  stimes = stimesIdempotentMonoid

instance Monoid HaskellCfg where
  mempty = StackGhci [] Nothing []

instance Interpreter HaskellCfg Haskell where
  interpreterLangs _ = ["hs", "haskell"]
  interpreterInfo = infoStr

  createInterpreter cfg = do
    let (cmd, args) = case cfg of
          StackGhci{hs_packages, hs_resolver, hs_args} -> 
            ("stack",
              ["ghci"]
              ++ maybe [] (\r -> ["--resolver", r]) hs_resolver
              ++ concatMap (\p -> ["--package", p]) hs_packages
              ++ hs_args)
          CustomGhci{hs_exe, hs_args} -> (hs_exe, hs_args)
        infoStr = unwords $ show cmd : args
    handles <- backendProc cmd args
    -- TODO: clean spam, implement other methods
    pure Haskell{cfg, infoStr, handles}
  
  freeInterpreter = cleanupHandles . handles

  runInterpreterOn _ _ _ = pure (Left "not implemented")