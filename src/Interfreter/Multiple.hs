{-# LANGUAGE UndecidableInstances #-}
module Interfreter.Multiple (Interpreters(..)) where

import Control.Applicative
import Data.Foldable
import Data.Char
import Data.Kind
import Data.List
import Data.Proxy

import Interfreter.Types
import Interfreter.Util.SymbolList

data Interpreters :: [Type] -> Type where
  Silent :: Interpreters '[]
  (:--)  :: () -> !(Interpreters is) -> Interpreters (i ': is)
  (:||)  :: !i -> !(Interpreters is) -> Interpreters (i ': is)

data MultiConfig :: [Type] -> Type where

instance Interpreter (Interpreters '[]) where
  type Langs (Interpreters '[]) = '[]
  type Config (Interpreters '[]) = MultiConfig '[]
  interpreterInfo s = ""
  createInterpreter = error "TODO"
  freeInterpreter _ = pure ()
  runInterpreterOn _ lang _ = pure (Right "")

instance
  (Interpreter i
  , Interpreter (Interpreters is)
  , KnownSymbolList (Langs i ++ Langs (Interpreters is))
  ) => Interpreter (Interpreters (i ': is)) where
  type Langs (Interpreters (i ': is)) =
    Langs i ++ Langs (Interpreters is)
  
  type Config (Interpreters (i ': is)) = MultiConfig (i ': is)

  interpreterInfo (_ :-- is) = interpreterInfo is
  interpreterInfo (i :|| is) = unlines $ lines (interpreterInfo i) <> lines (interpreterInfo is)

  createInterpreter = error "TODO"

  freeInterpreter (_ :-- is) = freeInterpreter is
  freeInterpreter (i :|| is) = freeInterpreter i >> freeInterpreter is

  runInterpreterOn (_ :-- is) lang code = if lang `elem` interpreterLangs' @i
    then pure (Left "")
    else runInterpreterOn is lang code
  runInterpreterOn (i :|| is) lang code = if lang `elem` interpreterLangs' @i
    then
      runInterpreterOn i  lang code >>= \case
        err@Left{} -> runInterpreterOn is lang code >>= \case
          Left{} -> pure err
          r      -> pure r
    else runInterpreterOn is lang code
