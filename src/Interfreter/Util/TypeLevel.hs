{-# LANGUAGE MagicHash, ScopedTypeVariables #-}
-- | Utilities for working with type-level
-- lists of strings. (with DataKinds + TypeLits)
module Interfreter.Util.TypeLevel
  (
    KnownSymbolList(..), symbolListVal
  -- * Re-exports from Data.Vinyl.TypeLevel
  , module Data.Vinyl.TypeLevel
  -- * Re-exports from "GHC.TypeLits"
  , Symbol, KnownSymbol, symbolVal, symbolVal'
  ) where

import Data.Proxy
import GHC.Exts (Proxy#, proxy#)
import GHC.TypeLits

import Data.Vinyl.TypeLevel

symbolListVal :: forall xs proxy. KnownSymbolList xs => proxy xs -> [String]
symbolListVal p = symbolListVal' (proxy# :: Proxy# xs)

-- | This class allows getting a type-level
-- string list's value-level representation.
class KnownSymbolList (xs :: [Symbol]) where
  symbolListVal' :: Proxy# xs -> [String]

instance KnownSymbolList '[] where
  symbolListVal' _ = []

instance (KnownSymbol x, KnownSymbolList xs) => KnownSymbolList (x ': xs) where
  symbolListVal' _ = symbolVal' (proxy# :: Proxy# x) : symbolListVal' (proxy# :: Proxy# xs)