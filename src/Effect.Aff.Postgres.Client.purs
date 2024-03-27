module Effect.Aff.Postgres.Client where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Data.Postgres.Raw (Raw)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Postgres.Client (Client)
import Effect.Postgres.Result (Result)
import Record (insert, modify)
import Type.Prelude (Proxy(..))

type QueryRaw = { text :: String, values :: Array Raw, name :: Nullable String, rowMode :: String }

foreign import connectImpl :: Client -> Effect (Promise Unit)
foreign import endImpl :: Client -> Effect (Promise Unit)
foreign import queryImpl :: QueryRaw -> Client -> Effect (Promise Result)

newtype Query = Query { text :: String, values :: Array Raw, name :: Maybe String }

queryToRaw :: Query -> QueryRaw
queryToRaw (Query r) =
  let
    name = Proxy @"name"
    rowMode = Proxy @"rowMode"
  in
    insert rowMode "array"
      $ modify name toNullable
      $ r

class AsQuery a where
  asQuery :: a -> Query

instance AsQuery Query where
  asQuery = identity

instance AsQuery String where
  asQuery text = Query { text, values: [], name: Nothing }

instance AsQuery (String /\ Array Raw) where
  asQuery (text /\ values) = Query { text, values, name: Nothing }

connect :: Client -> Aff Unit
connect = Promise.toAffE <<< connectImpl

end :: Client -> Aff Unit
end = Promise.toAffE <<< endImpl

query :: forall q. AsQuery q => q -> Client -> Aff Result
query q = Promise.toAffE <<< queryImpl (queryToRaw $ asQuery q)
