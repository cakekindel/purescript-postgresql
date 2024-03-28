module Effect.Aff.Postgres.Client where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, toNullable)
import Data.Postgres (smash)
import Data.Postgres.Raw (Raw)
import Data.Postgres.Result (class FromResult, Result, fromRow, rows, rowsAffected)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Postgres.Client (Client, Config, make)
import Prim.Row (class Union)
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

connected :: forall r trash. Union r trash (Config ()) => Record r -> Aff Client
connected c = do
  client <- liftEffect $ make c
  connect client
  pure client

connect :: Client -> Aff Unit
connect = Promise.toAffE <<< connectImpl

end :: Client -> Aff Unit
end = Promise.toAffE <<< endImpl

queryRaw :: forall q. AsQuery q => q -> Client -> Aff Result
queryRaw q = Promise.toAffE <<< queryImpl (queryToRaw $ asQuery q)

exec :: forall q. AsQuery q => q -> Client -> Aff Int
exec q = map (fromMaybe 0 <<< rowsAffected) <<< queryRaw q

query :: forall q r. AsQuery q => FromResult r => q -> Client -> Aff (Array r)
query q = traverse (liftEffect <<< smash <<< fromRow) <=< map rows <<< queryRaw q
