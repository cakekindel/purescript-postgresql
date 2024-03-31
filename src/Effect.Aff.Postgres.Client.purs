module Effect.Aff.Postgres.Client (connected, connect, end, exec, query, queryRaw, __connect, __end, __query, module X) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Functor (voidRight)
import Data.Maybe (fromMaybe)
import Data.Postgres (smash)
import Data.Postgres.Query (class AsQuery, QueryRaw, asQuery, __queryToRaw)
import Data.Postgres.Result (class FromRow, Result, fromRow, rows, rowsAffected)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Postgres.Client (Client, Config, make)
import Effect.Postgres.Client as X
import Prim.Row (class Union)

-- | Create a client and immediately connect it to the database
-- |
-- | The config parameter `r` is `Config` with all keys optional.
-- |
-- | This is a shorthand for `(voidRight <*> connect) =<< liftEffect (make cfg)`
connected :: forall r trash. Union r trash (Config ()) => Record r -> Aff Client
connected cfg = (voidRight <*> connect) =<< liftEffect (make cfg)

-- | Connects the client to the database
-- |
-- | <https://node-postgres.com/apis/client#clientconnect>
connect :: Client -> Aff Unit
connect = Promise.toAffE <<< __connect

-- | Disconnects the client from the database
-- |
-- | <https://node-postgres.com/apis/client#clientend>
end :: Client -> Aff Unit
end = Promise.toAffE <<< __end

-- | Performs a query, returning the raw `Result` object
-- |
-- | <https://node-postgres.com/apis/client#clientquery>
queryRaw :: forall q. AsQuery q => q -> Client -> Aff Result
queryRaw q c = do
  q' <- __queryToRaw <$> liftEffect (asQuery q)
  Promise.toAffE $ __query q' c

-- | Performs a query that we expect to not yield any rows,
-- | returning the number of rows affected by the statement.
-- |
-- | <https://node-postgres.com/apis/client#clientquery>
exec :: forall q. AsQuery q => q -> Client -> Aff Int
exec q = map (fromMaybe 0 <<< rowsAffected) <<< queryRaw q

-- | Performs a query that we expect to yield rows,
-- | returning them unmarshalled into destination type `r`.
-- |
-- | <https://node-postgres.com/apis/client#clientquery>
query :: forall q r. AsQuery q => FromRow r => q -> Client -> Aff (Array r)
query q = traverse (liftEffect <<< smash <<< fromRow) <=< map rows <<< queryRaw q

-- | FFI binding to `Client#connect`
foreign import __connect :: Client -> Effect (Promise Unit)

-- | FFI binding to `Client#end`
foreign import __end :: Client -> Effect (Promise Unit)

-- | FFI binding to `Client#query`
foreign import __query :: QueryRaw -> Client -> Effect (Promise Result)
