module Control.Monad.Postgres.Session where

import Prelude

import Control.Monad.Fork.Class (class MonadBracket)
import Control.Monad.Reader (class MonadReader, ReaderT, ask)
import Data.Postgres.Query (class AsQuery)
import Data.Postgres.Result (class FromRows)
import Effect.Aff (Fiber)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Postgres.Client (Client)
import Effect.Aff.Postgres.Client as Client
import Effect.Exception (Error)

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

instance MonadAff m => MonadSession (SessionT m) where
  query q = do
    client <- ask
    liftAff $ Client.query q client
  exec q = do
    client <- ask
    liftAff $ Client.exec q client
  exec_ = void <<< exec
