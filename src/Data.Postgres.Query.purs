module Data.Postgres.Query where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, toNullable)
import Data.Postgres.Raw (Raw)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Record (insert, modify)
import Type.Prelude (Proxy(..))

-- | SQL Query
-- |
-- | * `text` - the query string
-- | * `values` - query parameter values
-- | * `name` (optional) - providing this will create this query as a [prepared statement](https://node-postgres.com/features/queries#prepared-statements)
newtype Query = Query { text :: String, values :: Array Raw, name :: Maybe String }

derive instance Newtype Query _
derive newtype instance Show Query

-- | An empty query
emptyQuery :: Query
emptyQuery = Query { text: "", values: [], name: Nothing }

-- | Values that can be rendered as a SQL query
class AsQuery a where
  asQuery :: a -> Effect Query

instance AsQuery a => AsQuery (Effect a) where
  asQuery a = asQuery =<< a

instance AsQuery Query where
  asQuery = pure

instance AsQuery String where
  asQuery text = pure $ Query { text, values: [], name: Nothing }

instance AsQuery (String /\ Array Raw) where
  asQuery (text /\ values) = pure $ Query { text, values, name: Nothing }

-- | FFI
type QueryRaw = { text :: String, values :: Array Raw, name :: Nullable String, rowMode :: String }

-- | FFI
__queryToRaw :: Query -> QueryRaw
__queryToRaw (Query r) =
  let
    name = Proxy @"name"
    rowMode = Proxy @"rowMode"
  in
    insert rowMode "array" $ modify name toNullable $ r

