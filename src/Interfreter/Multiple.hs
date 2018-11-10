module Interfreter.Multiple where

import Interfreter.Types

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Applicative
import Data.Foldable
import Data.Char
import Data.Kind
import Data.Proxy

data Interpreters :: [Type] -> Type where
  Silent :: Interpreters '[]
  (:--)  :: () -> !(Interpreters is) -> Interpreters (i ': is)
  (:||)  :: !i -> !(Interpreters is) -> Interpreters (i ': is)

instance Interpreter (Interpreters '[]) where
  interpreterLangs _ = []
  interpreterInfo s = ""
  createInterpreter = createInterpretersViaCfg
  freeInterpreter _ = pure ()
  runInterpreterOn _ lang _ = pure (Right "")

instance (Interpreter i, Interpreter (Interpreters is), CreateInterpreters is) => Interpreter (Interpreters (i ': is)) where
  interpreterLangs _ = interpreterLangs' @i <> interpreterLangs' @(Interpreters is)

  interpreterInfo (_ :-- is) = interpreterInfo is
  interpreterInfo (i :|| is) = unlines $ lines (interpreterInfo i) <> lines (interpreterInfo is)

  createInterpreter = createInterpretersViaCfg

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

createInterpretersViaCfg :: CreateInterpreters is => String -> IO (Interpreters is)
createInterpretersViaCfg = createInterpreters . parseInterpretersCfg

class CreateInterpreters (is :: [Type]) where
  createInterpreters :: Map String String -> IO (Interpreters is)

instance CreateInterpreters '[] where
  createInterpreters m = pure Silent

instance (Interpreter i, CreateInterpreters is) => CreateInterpreters (i ': is) where
  createInterpreters m = do
    let ilangs = interpreterLangs' @i
    let icfg = asum $ (`Map.lookup` m) <$> ilangs
    i  <- traverse createInterpreter icfg
    is <- createInterpreters @is m
    pure $ case i of
      Nothing  -> () :-- is
      Just ip  -> ip :|| is

parseInterpretersCfg :: String -> Map String String
parseInterpretersCfg = error "TODO"