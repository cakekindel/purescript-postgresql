module Control.Monad.Postgres where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Fork.Class (class MonadBracket, bracket)
import Control.Monad.Reader (class MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Postgres.Query (class AsQuery)
import Data.Postgres.Result (class FromRows)
import Effect.Aff (Fiber)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Postgres.Client (Client)
import Effect.Aff.Postgres.Client as Client
import Effect.Aff.Postgres.Pool (Pool)
import Effect.Aff.Postgres.Pool as Pool
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Prim.Row (class Union)

-- | Monad handling pool resource acquisition & release
-- |
-- | ```
-- | runPostgres
-- |   {connectionString: "postgresql://postgres:postgres@localhost:5432"}
-- |   $ session do
-- |       exec_ "create table foo (bar int);"
-- |       exec_ "insert into foo values (1);"
-- |       res <- query "select * from foo"
-- |       pure $ res == 1
-- | ```
-- |
-- | Is equivalent to:
-- | ```
-- | do
-- |   pool <- liftEffect $ Pool.make {connectionString: "postgresql://postgres:postgres@localhost:5432"}
-- |   finally (Pool.end pool) do
-- |     client <- Pool.connect pool
-- |     finally (liftEffect $ Pool.release pool client) do
-- |       Client.exec_ "create table foo (bar int);" client
-- |       Client.exec_ "insert into foo values (1);" client
-- |       res <- Client.query "select * from foo" client
-- |       pure $ res == 1
-- | ```
type PostgresT :: forall k. (k -> Type) -> k -> Type
type PostgresT = ReaderT Pool

type SessionT :: forall k. (k -> Type) -> k -> Type
type SessionT = ReaderT Client

-- | A monad representing a connected session to a database
class (MonadBracket Error Fiber m, MonadAff m, MonadReader Client m) <= MonadSession m where
  -- | Executes a query and unmarshals the result into `r`
  query :: forall q r. AsQuery q => FromRows r => q -> m r
  -- | Executes a query and returns the number of rows affected
  exec :: forall q. AsQuery q => q -> m Int
  -- | Executes a query and discards the result
  exec_ :: forall q. AsQuery q => q -> m Unit

instance (MonadBracket Error Fiber m, MonadAff m, MonadReader Client m) => MonadSession m where
  query q = do
    client <- ask
    liftAff $ Client.query q client
  exec q = do
    client <- ask
    liftAff $ Client.exec q client
  exec_ = void <<< exec

-- | Lifts a session to `PostgresT`, releasing the client to the pool
-- | after execution.
session :: forall m a. MonadBracket Error Fiber m => MonadAff m => MonadSession (SessionT m) => SessionT m a -> PostgresT m a
session m = do
  pool <- ask
  let
    acq = liftAff $ Pool.connect pool
    rel _ c = liftEffect $ Pool.release pool c
  lift $ bracket acq rel (runReaderT m)

-- | Lifts a session to `PostgresT`, running the session
-- | in a transaction.
-- |
-- | If the session throws an error, the transaction will be
-- | rolled back and the error rethrown.
transaction :: forall m a. MonadBracket Error Fiber m => MonadAff m => MonadSession (SessionT m) => SessionT m a -> PostgresT m a
transaction m =
  let
    begin = void $ exec "begin;"
    commit = m <* exec "commit;"
    rollback e = exec "rollback;" *> throwError e
  in
    session $ begin *> catchError commit rollback

-- | Runs a `PostgresT` with a pool created with the provided config, invoking `Pool.end` afterwards.
runPostgres :: forall m a missing trash r e f. MonadBracket e f m => MonadAff m => Union r missing (Pool.Config trash) => Record r -> PostgresT m a -> m a
runPostgres cfg m =
  let
    acq = liftEffect $ Pool.make @r @missing @trash cfg
    rel _ p = liftAff $ Pool.end p
  in
    bracket acq rel $ runReaderT m
