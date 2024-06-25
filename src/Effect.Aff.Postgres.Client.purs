module Effect.Aff.Postgres.Client (connected, connect, end, exec, execWithStdin, queryWithStdout, query, queryRaw, __connect, __end, __query, __execStreamStdin, __execStreamStdout, module Reexport) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Functor (voidRight)
import Data.Maybe (fromMaybe)
import Data.Newtype (wrap)
import Data.Postgres.Query (class AsQuery, QueryRaw, __queryToRaw, asQuery, stringQuery)
import Data.Postgres.Result (class FromRows, Result, fromRows, rows, rowsAffected)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Postgres.Client (Client, ClientConfigRaw, Config, Notification, NotificationRaw, __make, __uncfg, endE, errorE, make, noticeE, notificationE) as Reexport
import Effect.Postgres.Client (Client, Config, make)
import Effect.Postgres.Error as E
import Effect.Postgres.Error.Except as X
import Node.Stream (Readable, Writable)
import Prim.Row (class Union)

-- | Create a client and immediately connect it to the database
-- |
-- | The config parameter `r` is `Config` with all keys optional.
-- |
-- | This is a shorthand for `(voidRight <*> connect) =<< liftEffect (make cfg)`
connected :: forall r missing trash. Union r missing (Config trash) => Record r -> E.Except Aff Client
connected cfg = X.with E.Connecting $ (voidRight <*> connect) =<< liftEffect (make @r @missing @trash cfg)

-- | Connects the client to the database
-- |
-- | <https://node-postgres.com/apis/client#clientconnect>
connect :: Client -> Aff Unit
connect = Promise.toAffE <<< __connect

-- | Disconnects the client from the database
-- |
-- | <https://node-postgres.com/apis/client#clientend>
end :: Client -> E.Except Aff Unit
end = X.with E.Disconnecting <<< Promise.toAffE <<< __end

-- | Performs a query, returning the raw `Result` object
-- |
-- | <https://node-postgres.com/apis/client#clientquery>
queryRaw :: forall q. AsQuery q => q -> Client -> E.Except Aff Result
queryRaw q c = do
  q' <- X.printing $ asQuery q
  let q'' = __queryToRaw q'
  X.executing q' $ Promise.toAffE $ __query q'' c

-- | Performs a query that we expect to not yield any rows,
-- | returning the number of rows affected by the statement.
-- |
-- | <https://node-postgres.com/apis/client#clientquery>
exec :: forall q. AsQuery q => q -> Client -> E.Except Aff Int
exec q = map (fromMaybe 0 <<< rowsAffected) <<< queryRaw q

-- | Performs a query that we expect to yield rows,
-- | returning them unmarshalled into destination type `r`.
-- |
-- | <https://node-postgres.com/apis/client#clientquery>
query :: forall q r. AsQuery q => FromRows r => q -> Client -> E.Except Aff r
query q c = do
  q' <- X.printing $ asQuery q
  raw <- queryRaw q c
  let
    affected = rowsAffected raw
    rows' = rows raw
  X.parsing q' $ fromRows (wrap $ fromMaybe 0 affected) rows'

execWithStdin :: String -> Client -> E.Except Effect (Writable ())
execWithStdin q c = X.executing (stringQuery q) $ __execStreamStdin q c

queryWithStdout :: String -> Client -> E.Except Effect (Readable ())
queryWithStdout q c = X.executing (stringQuery q) $ __execStreamStdout q c

-- | FFI binding to `Client#connect`
foreign import __connect :: Client -> Effect (Promise Unit)

-- | FFI binding to `Client#end`
foreign import __end :: Client -> Effect (Promise Unit)

-- | FFI binding to `Client#query`
foreign import __query :: QueryRaw -> Client -> Effect (Promise Result)

-- | FFI binding to `import('pg-copy-streams').to`
foreign import __execStreamStdout :: String -> Client -> Effect (Readable ())

-- | FFI binding to `import('pg-copy-streams').from`
foreign import __execStreamStdin :: String -> Client -> Effect (Writable ())
