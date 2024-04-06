module Data.Postgres.Unresult where

import Prelude

import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.State (StateT, runStateT, state)
import Data.Array as Array
import Data.Postgres (class Deserialize, RepT, deserialize, smash)
import Data.Postgres.Raw (Raw)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Foreign (ForeignError(..))

-- | Monad used to incrementally deserialize columns from a row
type Unresult a = StateT {ix :: Int, row :: Array Raw} RepT a

-- | Run an `UnresultT`
unresult :: forall a. Unresult a -> Array Raw -> RepT a
unresult m row = fst <$> runStateT m {ix: 0, row}

-- | Take the next column from the row, unmarshalling into `a`
take :: forall a. Deserialize a => Unresult a
take = do
  raw <- state (\r -> Array.index r.row r.ix /\ r {ix = r.ix + 1})
  raw' <- liftMaybe (pure $ ForeignError $ "Ran out of columns!") raw
  liftEffect $ smash $ deserialize raw'
