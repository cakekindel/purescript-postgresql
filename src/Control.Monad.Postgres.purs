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

type PoolT :: forall k. (k -> Type) -> k -> Type
type PoolT = ReaderT Pool

type SessionT :: forall k. (k -> Type) -> k -> Type
type SessionT = ReaderT Client

class (MonadBracket Error Fiber m, MonadAff m, MonadReader Client m) <= MonadSession m where
  query :: forall q r. AsQuery q => FromRows r => q -> m r
  exec :: forall q. AsQuery q => q -> m Int
  exec_ :: forall q. AsQuery q => q -> m Unit

instance (MonadBracket Error Fiber m, MonadAff m, MonadReader Client m) => MonadSession m where
  query q = do
    client <- ask
    liftAff $ Client.query q client
  exec q = do
    client <- ask
    liftAff $ Client.exec q client
  exec_ = void <<< exec

session :: forall m a. MonadBracket Error Fiber m => MonadAff m => MonadSession (SessionT m) => SessionT m a -> PoolT m a
session m = do
  pool <- ask
  let
    acq = liftAff $ Pool.connect pool
    rel _ c = liftEffect $ Pool.release pool c
  lift $ bracket acq rel (runReaderT m)

transaction :: forall m a. MonadBracket Error Fiber m => MonadAff m => MonadSession (SessionT m) => SessionT m a -> PoolT m a
transaction m =
  let
    begin = void $ exec "begin;"
    commit = m <* exec "commit;"
    rollback e = exec "rollback;" *> throwError e
  in
    session $ begin *> catchError commit rollback

runPool :: forall m a missing trash r e f. MonadBracket e f m => MonadAff m => Union r missing (Pool.Config trash) => Record r -> PoolT m a -> m a
runPool cfg m =
  let
    acq = liftEffect $ Pool.make @r @missing @trash cfg
    rel _ p = liftAff $ Pool.end p
  in
    bracket acq rel (runReaderT m)
