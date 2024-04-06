module Data.Postgres.Unresult where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, liftMaybe)
import Control.Monad.Morph (hoist)
import Control.Monad.State (StateT(..), runStateT, state)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Maybe (fromMaybe, maybe)
import Data.Postgres (class Deserialize, class Rep, RepT, deserialize, smash)
import Data.Postgres.Raw (Raw)
import Data.Postgres.Result (fromRow)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect.Aff (error)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)

-- | Monad used to incrementally deserialize columns from a row
type UnresultT m a = StateT {ix :: Int, row :: Array Raw} m a

-- | Run an `UnresultT`
unresult :: forall m a. Monad m => Array Raw -> UnresultT m a -> m a
unresult row m = fst <$> runStateT m {ix: 0, row}

-- | Take the next column from the row, unmarshalling into `a`
take :: forall m a. MonadThrow Error m => Deserialize a => MonadEffect m => UnresultT m a
take = do
  raw <- state (\r -> Array.index r.row r.ix /\ r {ix = r.ix + 1})
  raw' <- liftMaybe (error "Ran out of columns!") raw
  liftEffect $ smash $ deserialize raw'
