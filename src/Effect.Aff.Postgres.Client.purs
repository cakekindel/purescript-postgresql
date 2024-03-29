module Effect.Aff.Postgres.Client where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Maybe (fromMaybe)
import Data.Postgres (smash)
import Data.Postgres.Query (class AsQuery, QueryRaw, asQuery, queryToRaw)
import Data.Postgres.Result (class FromResult, Result, fromRow, rows, rowsAffected)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Postgres.Client (Client, Config, make)
import Prim.Row (class Union)

foreign import connectImpl :: Client -> Effect (Promise Unit)
foreign import endImpl :: Client -> Effect (Promise Unit)
foreign import queryImpl :: QueryRaw -> Client -> Effect (Promise Result)

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
queryRaw q c = do
  q' <- queryToRaw <$> liftEffect (asQuery q)
  Promise.toAffE $ queryImpl q' c

exec :: forall q. AsQuery q => q -> Client -> Aff Int
exec q = map (fromMaybe 0 <<< rowsAffected) <<< queryRaw q

query :: forall q r. AsQuery q => FromResult r => q -> Client -> Aff (Array r)
query q = traverse (liftEffect <<< smash <<< fromRow) <=< map rows <<< queryRaw q
