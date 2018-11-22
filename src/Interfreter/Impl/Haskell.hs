{-# LANGUAGE ViewPatterns #-}
module Interfreter.Impl.Haskell (Haskell(..)) where

import Control.Applicative
import Data.Semigroup

import System.Process.Typed

import Interfreter.Types
import Interfreter.Util.Process
import Interfreter.Util.Impl

data Haskell
  = Haskell
    { cfg     :: !HaskellCfg
    , infoStr :: !String
    , procCfg :: !(ProcessConfig Handle Handle Handle)
    , theGhci :: !(Process Handle Handle Handle)
    }

data HaskellCfg
  = HaskellCfg
    { ghciCommand :: !GhciCommand
    , ghciOpts    :: !GhciOpts
    , ghciWrapper :: !GhciWrapper
    }

data GhciCommand
  = PlainGhci
  | CabalNewRepl !CabalOpts
  | StackGhci !StackOpts
  | CustomCommand !FilePath ![String]

ghciCommandOptsToCmd :: GhciOpts -> GhciCommand -> Cmd
ghciCommandOptsToCmd (ghciOptsToArgList -> args) = \case
  PlainGhci             ->
    Cmd "ghci" args
  CabalNewRepl opts     ->
    Cmd "cabal" $ concat [ cabalOptsToArgs opts
                         , ["v2-repl"] 
                         , args >>= \a -> ["--repl-option", a]
                         ]
  StackGhci opts        ->
    Cmd "stack" $ concat [ stackOptsToArgs opts
                         , ["repl"]
                         , args >>= \a -> ["--ghci-options", a]
                         ]
  CustomCommand fp args -> Cmd fp args

data GhciOpts
  = GhciOpts

ghciOptsToArgList :: GhciOpts -> [String]
ghciOptsToArgList _ = []

data GhciWrapper
  = NoWrapper
  | StackExecWrapper !StackOpts
  | CabalNewRunWrapper !CabalOpts

data CabalOpts
  = CabalOpts

cabalOptsToArgs :: CabalOpts -> [String]
cabalOptsToArgs _ = []

data StackOpts
  = StackOpts
    { stackResolver :: !StackResolver
    }

stackOptsToArgs :: StackOpts -> [String]
stackOptsToArgs opts = concat
  [ case stackResolver opts of
      InferResolver -> []
      LTSResolver maj min ->
        ["--resolver", "lts-" <> show maj <> "." <> show min]
      NightlyResolver y m d ->
        ["--resolver", "nightly-" <> show y <> "-" <> show m <> "-" <> show d]
      OtherResolver res ->
        ["--resolver", res]
  ]

data StackResolver = InferResolver
                   | LTSResolver !Int !Int -- ^ major minor
                   | NightlyResolver !Int !Int !Int -- ^ year month day
                   | OtherResolver !String

instance Interpreter Haskell where
  type Langs Haskell = '["hs", "haskell"]

  type Config Haskell = HaskellCfg -- for now

  interpreterInfo = infoStr

  createInterpreter cfg = do
    let cmd = constructGhciInvoke cfg
    error "TODO"
  
  freeInterpreter = defaultFreeInterpreter theGhci

  runInterpreterOn = error "TODO"

constructGhciInvoke :: HaskellCfg -> Cmd
constructGhciInvoke cfg = ghciCommandOptsToCmd (ghciOpts cfg) (ghciCommand cfg)