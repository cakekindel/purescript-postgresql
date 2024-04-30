module Control.Monad.Postgres.Session where

import Prelude

import Control.Monad.Reader (ReaderT, ask)
import Data.Postgres.Query (class AsQuery)
import Data.Postgres.Result (class FromRows)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Postgres.Client (Client)
import Effect.Aff.Postgres.Client as Client
import Effect.Class (liftEffect)
import Node.Stream (Writable, Readable)

type SessionT :: forall k. (k -> Type) -> k -> Type
type SessionT = ReaderT Client

-- | A monad representing a connected session to a database
class MonadAff m <= MonadSession m where
  -- | Executes a query and unmarshals the result into `r`
  query :: forall q r. AsQuery q => FromRows r => q -> m r
  -- | Executes a query and returns the number of rows affected
  exec :: forall q. AsQuery q => q -> m Int
  -- | Executes a query and discards the result
  exec_ :: forall q. AsQuery q => q -> m Unit
  -- | Execute a query with a `Writable` stream to `STDIN`
  -- |
  -- | Use with `COPY .. FROM` like so:
  -- |
  -- | ```purescript
  -- | w <- streamIn "COPY foo FROM STDIN WITH (FORMAT CSV, HEADER true)"
  -- | liftEffect $ Stream.writeString "bar\n\"my bar column\"" UTF8 w
  -- | ```
  streamIn :: String -> m (Writable ())
  -- | Execute a query with a `Readable` stream from `STDOUT`
  -- |
  -- | Use with `COPY .. TO` like so:
  -- |
  -- | ```purescript
  -- | r <- streamIn "COPY foo TO STDIN WITH (FORMAT CSV, HEADER true)"
  -- | liftEffect $ Stream.readString r -- "bar\n\"my bar column\""
  -- | ```
  streamOut :: String -> m (Readable ())

instance MonadAff m => MonadSession (SessionT m) where
  query q = do
    client <- ask
    liftAff $ Client.query q client
  exec q = do
    client <- ask
    liftAff $ Client.exec q client
  exec_ = void <<< exec
  streamIn q = do
    client <- ask
    liftEffect $ Client.execWithStdin q client
  streamOut q = do
    client <- ask
    liftEffect $ Client.queryWithStdout q client
