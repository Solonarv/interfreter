{-# LANGUAGE MagicHash, ScopedTypeVariables #-}
-- | Utilities for working with type-level
-- lists of strings. (with DataKinds + TypeLits)
module Interfreter.Util.SymbolList
  (
    KnownSymbolList(..)
  , symbolListVal, symbolListVal'
  , type (++)
  -- * Re-exports from "GHC.TypeLits"
  , Symbol, KnownSymbol, symbolVal, symbolVal'
  ) where

import Data.Proxy
import GHC.Exts (Proxy#, proxy#)
import GHC.TypeLits

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

-- | Append two type-level lists.
type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[]       ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)