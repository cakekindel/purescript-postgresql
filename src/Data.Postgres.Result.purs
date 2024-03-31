module Data.Postgres.Result where

import Prelude

import Control.Monad.Error.Class (liftMaybe, throwError)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Postgres (class Rep, RepT, deserialize)
import Data.Postgres.Raw (Raw)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import Foreign (ForeignError(..))
import Type.Prelude (Proxy(..))

-- | A raw query result
-- |
-- | <https://node-postgres.com/apis/result>
foreign import data Result :: Type

-- | Returns the number of rows affected by the query
-- |
-- | <https://node-postgres.com/apis/result#resultrowcount-int--null>
rowsAffected :: Result -> Maybe Int
rowsAffected = Int.fromNumber <=< Nullable.toMaybe <<< __rowsAffected

-- | Can be unmarshalled from a queried row
-- |
-- | Implementations are provided for:
-- |  * tuples of any length containing types that are `Rep`
-- |  * tuples of any length with the last member of `Array Raw`
-- |  * a single value of a type that is `Rep`
-- |  * `Array Raw`
-- |  * `Unit` (always succeeds)
-- |
-- | ```
-- | -- CREATE TABLE foo
-- | --   ( id INT NOT NULL PRIMARY KEY
-- | --   , fruit TEXT NOT NULL
-- | --   , created TIMESTAMPTZ NOT NULL DEFAULT NOW()
-- | --   );
-- | do
-- |   let q = query "select id, fruit, created from foo" client
-- |
-- |   -- pick all 3 columns explicitly
-- |   _ :: Array (Int /\ String /\ DateTime) <- q
-- |
-- |   -- pick first 2 columns, discarding any others
-- |   _ :: Array (Int /\ String) <- q
-- |
-- |   -- pick first 2 columns, if any more keep as `Array Raw`
-- |   _ :: Array (Int /\ String /\ Array Raw) <- q
-- |
-- |   -- pick just the ID, discarding all other columns
-- |   id :: Array Int <- q
-- |
-- |   pure unit
-- | ```
class FromRow (a :: Type) where
  -- | Minimum length of row for type `a`
  minColumnCount :: forall g. g a -> Int
  -- | Performs the conversion
  fromRow :: Array Raw -> RepT a

instance (Rep a, FromRow b) => FromRow (a /\ b) where
  minColumnCount _ = minColumnCount (Proxy @b) + 1
  fromRow r =
    let
      minLen = minColumnCount (Proxy @(Tuple a b))
      lengthMismatch = pure $ TypeMismatch ("Expected row to have at least " <> show minLen <> " columns") ("Found row of length " <> show (Array.length r))
    in
      do
        when (Array.length r < minLen) (throwError lengthMismatch)
        a <- deserialize =<< liftMaybe lengthMismatch (Array.head r)
        b <- fromRow =<< liftMaybe lengthMismatch (Array.tail r)
        pure $ a /\ b
else instance FromRow (Array Raw) where
  minColumnCount _ = 0
  fromRow = pure
else instance FromRow Unit where
  minColumnCount _ = 0
  fromRow _ = pure unit
else instance Rep a => FromRow a where
  minColumnCount _ = 1
  fromRow r =
    let
      err = pure $ TypeMismatch "Expected row of length >= 1" "Empty row"
    in
      deserialize =<< liftMaybe err (Array.head r)

-- | FFI binding for `Result#rowCount`
foreign import __rowsAffected :: Result -> Nullable Number

-- | FFI binding for `Result#rows`
foreign import rows :: Result -> Array (Array Raw)
