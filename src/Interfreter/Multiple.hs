{-# LANGUAGE UndecidableInstances #-}
module Interfreter.Multiple
  ( CtMaybe(..)
  , Multi(..)
  , CfgFor(..)
  , MultiCfg(..)
  ) where

import Control.Applicative
import Data.Foldable
import Data.Char
import Data.Kind
import Data.List
import Data.Proxy

import Data.Vinyl
import Data.Vinyl.Core

import Interfreter.Types
import Interfreter.Util.TypeLevel

data CtMaybe c i where
  CtSome :: c i => i -> CtMaybe c i
  CtNone :: CtMaybe c i

newtype Multi is = Multi { unMulti :: Rec (CtMaybe Interpreter) is }

data CfgFor i where
  ACfg :: Interpreter i => Config i -> CfgFor i
  NoCfg :: CfgFor i

newtype MultiCfg is = MultiCfg { unMultiCfg :: Rec CfgFor is }

instance ( AllConstrained Interpreter is
         , KnownSymbolList (CatLangs is)
         , RFoldMap is
         ) => Interpreter (Multi is) where
  type Langs  (Multi is) = CatLangs is
  type Config (Multi is) = MultiCfg is
  interpreterInfo = unlines . rfoldMap interpInfoAux . unMulti
  createInterpreter = fmap Multi . rtraverse mkInterpAux . unMultiCfg
  freeInterpreter = rfoldMap freeInterpAux . unMulti
  runInterpreterOn _ lang _ = error "TODO"

interpInfoAux :: CtMaybe Interpreter i -> [String]
interpInfoAux (CtSome i) = [interpreterInfo i]
interpInfoAux CtNone = []

mkInterpAux :: CfgFor i -> IO (CtMaybe Interpreter i)
mkInterpAux (ACfg cfg) = CtSome <$> createInterpreter cfg
mkInterpAux NoCfg = pure CtNone

freeInterpAux :: CtMaybe Interpreter i -> IO ()
freeInterpAux (CtSome i) = freeInterpreter i
freeInterpAux CtNone = pure ()

type family CatLangs (xs :: [Type]) :: [Symbol] where
  CatLangs '[]       = '[]
  CatLangs (x ': xs) = Langs x ++ CatLangs xs