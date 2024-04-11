module Control.Monad.Postgres.Base where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Plus)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Fork.Class (class MonadBracket, class MonadFork, class MonadKill, bracket, kill, never, uninterruptible)
import Control.Monad.Morph (class MFunctor, class MMonad)
import Control.Monad.Postgres.Session (class MonadSession, SessionT, exec, exec_, query)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, local, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Parallel (class Parallel, parallel, sequential)
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect.Aff (Fiber)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Postgres.Pool (Pool)
import Effect.Aff.Postgres.Pool as Pool
import Effect.Aff.Unlift (class MonadUnliftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Effect.Unlift (class MonadUnliftEffect)
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
newtype PostgresT :: forall k. (k -> Type) -> k -> Type
newtype PostgresT m a = PostgresT (ReaderT Pool m a)

derive instance Newtype (PostgresT m a) _
derive newtype instance (Functor m) => Functor (PostgresT m)
derive newtype instance (Apply m) => Apply (PostgresT m)
derive newtype instance (Applicative m) => Applicative (PostgresT m)
derive newtype instance (Plus m) => Plus (PostgresT m)
derive newtype instance (Alt m) => Alt (PostgresT m)
derive newtype instance (Bind m) => Bind (PostgresT m)
derive newtype instance (Monad m) => Monad (PostgresT m)
derive newtype instance (MonadEffect m) => MonadEffect (PostgresT m)
derive newtype instance (MonadAff m) => MonadAff (PostgresT m)
derive newtype instance (MonadUnliftEffect m) => MonadUnliftEffect (PostgresT m)
derive newtype instance (MonadUnliftAff m) => MonadUnliftAff (PostgresT m)
derive newtype instance MonadRec m => MonadRec (PostgresT m)
derive newtype instance MonadTrans (PostgresT)
derive newtype instance (MonadThrow e m) => MonadThrow e (PostgresT m)
derive newtype instance (MonadError e m) => MonadError e (PostgresT m)
derive newtype instance (MonadFork f m) => MonadFork f (PostgresT m)
derive newtype instance MFunctor PostgresT
derive newtype instance MMonad PostgresT
instance (Apply m, Apply p, Parallel p m) => Parallel (PostgresT p) (PostgresT m) where
  parallel = wrap <<< parallel <<< unwrap
  sequential = wrap <<< sequential <<< unwrap

instance (Monad m, MonadKill e f m) => MonadKill e f (PostgresT m) where
  kill a b = lift $ kill a b

instance (Monad m, MonadBracket e f (ReaderT Pool m), MonadBracket e f m) => MonadBracket e f (PostgresT m) where
  bracket acq rel m = wrap $ bracket (unwrap acq) (\a b -> unwrap $ rel a b) (unwrap <<< m)
  uninterruptible a = wrap $ uninterruptible $ unwrap a
  never = lift $ never

instance Monad m => MonadAsk Pool (PostgresT m) where
  ask = wrap ask

instance Monad m => MonadReader Pool (PostgresT m) where
  local f m = wrap $ local f $ unwrap m

instance (MonadBracket e f m, MonadAff m) => MonadSession (PostgresT m) where
  query = session <<< query
  exec = session <<< exec
  exec_ = session <<< exec_

-- | Lifts a session to `PostgresT`, releasing the client to the pool
-- | after execution.
session :: forall e f m a. MonadBracket e f m => MonadAff m => MonadSession (SessionT m) => SessionT m a -> PostgresT m a
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

-- | Execute a `PostgresT` using an existing connection pool.
-- |
-- | This will not invoke `Pool.end` after executing.
withPool :: forall m a. PostgresT m a -> Pool -> m a
withPool = runReaderT <<< unwrap

-- | Create a new connection pool from the provided config and execute
-- | the postgres monad, invoking `Effect.Aff.Postgres.Pool.end` afterwards.
runPostgres :: forall m a missing trash r e f. MonadBracket e f m => MonadAff m => Union r missing (Pool.Config trash) => Record r -> PostgresT m a -> m a
runPostgres cfg m =
  let
    acq = liftEffect $ Pool.make @r @missing @trash cfg
    rel _ p = liftAff $ Pool.end p
  in
    bracket acq rel $ withPool m
