module Control.Monad.Postgres.Cursor where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Plus)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Fork.Class (class MonadBracket, class MonadFork, class MonadKill, bracket, kill, never, uninterruptible)
import Control.Monad.Postgres.Base (PostgresT, transaction)
import Control.Monad.Postgres.Session (class MonadSession, SessionT, exec, exec_, query)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, local, runReaderT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Parallel (class Parallel, parallel, sequential)
import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Postgres.Query (class AsQuery, asQuery)
import Data.Postgres.Result (class FromRow)
import Effect.Aff (Fiber)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)

newtype CursorT :: forall k. Type -> (k -> Type) -> k -> Type
newtype CursorT t m a = CursorT (ReaderT String m a)

derive instance Newtype (CursorT t m a) _
derive newtype instance (Functor m) => Functor (CursorT t m)
derive newtype instance (Apply m) => Apply (CursorT t m)
derive newtype instance (Applicative m) => Applicative (CursorT t m)
derive newtype instance (Plus m) => Plus (CursorT t m)
derive newtype instance (Alt m) => Alt (CursorT t m)
derive newtype instance (Bind m) => Bind (CursorT t m)
derive newtype instance (Monad m) => Monad (CursorT t m)
derive newtype instance (MonadEffect m) => MonadEffect (CursorT t m)
derive newtype instance (MonadAff m) => MonadAff (CursorT t m)
derive newtype instance MonadTrans (CursorT t)
derive newtype instance (MonadThrow e m) => MonadThrow e (CursorT t m)
derive newtype instance (MonadError e m) => MonadError e (CursorT t m)
derive newtype instance (MonadFork f m) => MonadFork f (CursorT t m)
instance (Monad m, MonadKill e f m) => MonadKill e f (CursorT t m) where
  kill a b = lift $ kill a b

instance (Monad m, MonadBracket e f (ReaderT String m), MonadBracket e f m) => MonadBracket e f (CursorT t m) where
  bracket acq rel m = wrap $ bracket (unwrap acq) (\a b -> unwrap $ rel a b) (unwrap <<< m)
  uninterruptible a = wrap $ uninterruptible $ unwrap a
  never = lift $ never

instance Monad m => MonadAsk String (CursorT t m) where
  ask = wrap ask

instance Monad m => MonadReader String (CursorT t m) where
  local f m = wrap $ local f $ unwrap m

instance (Apply m, Apply p, Parallel p m) => Parallel (CursorT t p) (CursorT t m) where
  parallel = wrap <<< parallel <<< unwrap
  sequential = wrap <<< sequential <<< unwrap

-- | A monad representing a handle to a server-side cursor
-- |
-- | ```
-- | runPostgres {connectionString: "..."} do
-- |   exec_ "create table foo (id int not null primary key);"
-- |   exec_
-- |     $ intercalate "\n "
-- |       [ "insert into foo (id)"
-- |       , "values"
-- |       , intercalate ", "
-- |           $ map (\n -> "(" <> show n <> ")")
-- |           $ Array.range 1 100
-- |       ]
-- |
-- |   cursor @Int "foo_cursor" "select id from foo" do
-- |     a <- fetchOne -- 1
-- |     b <- fetchOne -- 2
-- |     c <- fetchOne -- 3
-- |     d <- fetch 10 -- 4..14
-- |     e <- fetchAll -- 15..100
-- |     pure unit
-- | ```
class (MonadSession m, FromRow t) <= MonadCursor m t where
  -- | Fetch a specified number of rows from the cursor
  fetch :: Int -> m (Array t)
  -- | Fetch all remaining rows from the cursor
  fetchAll :: m (Array t)

instance (FromRow t, MonadSession m) => MonadCursor (CursorT t m) t where
  fetch n = do
    cur <- ask
    query $ "fetch forward " <> show n <> " from " <> cur
  fetchAll = do
    cur <- ask
    query $ "fetch all from " <> cur

instance (MonadSession m) => MonadSession (CursorT t m) where
  query = lift <<< query
  exec = lift <<< exec
  exec_ = lift <<< exec_

-- | Fetch the next row from the cursor
fetchOne :: forall m t. MonadCursor m t => m (Maybe t)
fetchOne = Array.head <$> fetch 1

-- | Create a server-side cursor for a query in a transaction,
-- | and execute a `CursorT` with a view to the new cursor.
cursor :: forall m @t a q. FromRow t => AsQuery q => MonadAff m => MonadBracket Error Fiber m => MonadSession (SessionT m) => String -> q -> CursorT t (SessionT m) a -> PostgresT m a
cursor cur q m =
  transaction do
    q' <- liftEffect $ asQuery q
    exec_ $ "declare " <> cur <> " cursor for (" <> (unwrap q').text <> ");"
    runReaderT (unwrap m) cur
