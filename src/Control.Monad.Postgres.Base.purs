module Control.Monad.Postgres.Base where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Fork.Class (class MonadBracket, bracket)
import Control.Monad.Morph (hoist)
import Control.Monad.Postgres.Cursor (class MonadCursor, CursorT)
import Control.Monad.Postgres.Session (class MonadSession, SessionT, endSession, exec, exec_, startSession)
import Control.Monad.Reader (ask, runReaderT)
import Data.Newtype (unwrap)
import Data.Postgres (RepT)
import Data.Postgres.Query (class AsQuery, asQuery)
import Data.Postgres.Raw (Raw)
import Data.Postgres.Result (class FromRow, fromRow)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Postgres.Pool (Pool)
import Effect.Aff.Postgres.Pool as Pool
import Effect.Class (liftEffect)
import Effect.Exception as Effect
import Effect.Postgres.Error (RE)
import Effect.Postgres.Error as E
import Effect.Postgres.Error.Except as X
import Effect.Postgres.Error.RE as RE
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
type PostgresT :: (Type -> Type) -> Type -> Type
type PostgresT = RE Pool

-- | Typeclass generalizing `PostgresT`. Allows for dependency-injecting different
-- | implementations of the idea of a postgres connection.
-- |
-- | - `session` - Session monad (for `PostgresT` this is `SessionT`)
-- | - `cursor` - Cursor session monad (for `PostgresT` this is `CursorT`)
-- | - `ct` - Open type parameter for cursor type. Don't pin this to a concrete type.
class (Monad m, MonadSession session, MonadCursor cursor ct) <= MonadPostgres m session cursor ct | m -> ct cursor session where
  -- | Run a session in `m`.
  session :: session ~> m
  -- | Run a session in `m`, wrapped in a transaction.
  -- |
  -- | If any errors are raised, the transaction is rolled back and
  -- | the error rethrown.
  transaction :: session ~> m
  -- | `cursor`, but using a custom deserialize function for the data
  -- | yielded by the cursor
  cursorWith :: forall q. AsQuery q => (Array Raw -> RepT ct) -> String -> q -> cursor ~> m

instance
  ( MonadAff m
  , MonadSession (SessionT m)
  , MonadCursor (CursorT t (SessionT m)) t
  ) =>
  MonadPostgres
    (PostgresT m)
    (SessionT m)
    (CursorT ct (SessionT m))
    ct
  where
  session m = do
    pool <- ask
    client <- RE.liftExcept $ hoist liftAff $ startSession pool
    RE.finally
      (RE.liftExcept $ hoist liftEffect $ endSession pool client)
      (RE.liftExcept $ RE.toExcept m client)
  transaction m =
    let
      begin = void $ exec "begin;"
      commit = m <* exec "commit;"
      rollback e = exec "rollback;" *> throwError e
    in
      session $ begin *> catchError commit rollback
  cursorWith f cur q m =
    transaction do
      q' <- RE.liftExcept $ X.printing $ asQuery q
      exec_ $ "declare " <> cur <> " cursor for (" <> (unwrap q').text <> ");"
      runReaderT (unwrap m) (cur /\ f)

-- | Create a server-side cursor for a query in a transaction,
-- | and execute a `CursorT` with a view to the new cursor.
cursor :: forall @cursort t session cursor q a. MonadPostgres t session cursor cursort => AsQuery q => FromRow cursort => String -> q -> cursor a -> t a
cursor = cursorWith (fromRow 0)

-- | Execute a `PostgresT` using an existing connection pool.
-- |
-- | This will not invoke `Pool.end` after executing.
withPool :: forall m a. PostgresT m a -> Pool -> E.Except m a
withPool m p = runReaderT (unwrap m) p

-- | Create a new connection pool from the provided config and execute
-- | the postgres monad, invoking `Effect.Aff.Postgres.Pool.end` afterwards.
runPostgres :: forall m a missing trash r f. MonadBracket Effect.Error f m => MonadAff m => Union r missing (Pool.Config trash) => Record r -> PostgresT m a -> E.Except m a
runPostgres cfg m =
  let
    acq :: RE Unit m Pool
    acq = liftEffect $ Pool.make @r @missing @trash cfg

    rel :: _ -> Pool -> RE Unit m Unit
    rel _ p = RE.liftExcept $ hoist liftAff $ Pool.end p
  in
    RE.toExcept (bracket acq rel $ RE.liftExcept <<< withPool m) unit
