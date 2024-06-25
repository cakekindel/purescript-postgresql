module Control.Monad.Postgres.Session where

import Prelude hiding (join)

import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Control.Monad.Fork.Class (class MonadBracket)
import Control.Monad.Morph (hoist)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Data.Newtype (wrap)
import Data.Postgres.Query (class AsQuery)
import Data.Postgres.Result (class FromRows)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Postgres.Client (Client)
import Effect.Aff.Postgres.Client as Client
import Effect.Aff.Postgres.Pool (Pool)
import Effect.Aff.Postgres.Pool as Pool
import Effect.Aff.Unlift (class MonadUnliftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception as Effect
import Effect.Postgres.Error (RE)
import Effect.Postgres.Error as E
import Effect.Postgres.Error.Except as X
import Effect.Postgres.Error.RE as RE
import Effect.Ref as Ref
import Node.EventEmitter as Event
import Node.Stream (Readable, Stream, Writable)
import Node.Stream as Stream

type SessionT :: (Type -> Type) -> Type -> Type
type SessionT = RE Client

class MonadStartSession a where
  startSession :: a -> E.Except Aff Client
  endSession :: a -> Client -> E.Except Effect Unit

instance MonadStartSession Pool where
  startSession = Pool.connect
  endSession p c = Pool.release p c

instance MonadStartSession Client where
  startSession = pure
  endSession _ _ = pure unit

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

instance (MonadStartSession s, MonadAff m) => MonadSession (RE s m) where
  query q = do
    pool <- ask
    client <- hoist liftAff $ RE.liftExcept $ startSession pool
    RE.finally
      (hoist liftEffect $ RE.liftExcept $ endSession pool client)
      (wrap $ lift $ hoist liftAff $ Client.query q client)
  exec q = do
    pool <- ask
    client <- hoist liftAff $ RE.liftExcept $ startSession pool
    RE.finally
      (hoist liftEffect $ RE.liftExcept $ endSession pool client)
      (wrap $ lift $ hoist liftAff $ Client.exec q client)
  exec_ = void <<< exec
  streamIn q = do
    pool <- ask
    client <- hoist liftAff $ RE.liftExcept $ startSession pool
    handleStream (X.run $ endSession pool client) (RE.liftExcept $ hoist liftEffect $ Client.execWithStdin q client)
  streamOut q = do
    pool <- ask
    client <- hoist liftAff $ RE.liftExcept $ startSession pool
    handleStream (X.run $ endSession pool client) (RE.liftExcept $ hoist liftEffect $ Client.queryWithStdout q client)

handleStream :: forall e m r. MonadEffect m => MonadError e m => Effect Unit -> m (Stream r) -> m (Stream r)
handleStream onError getStream =
    flip catchError
      (\e -> liftEffect onError *> throwError e)
      do
        stream <- getStream
        liftEffect $ onErrorOrClose stream onError
        pure stream

onErrorOrClose :: forall r. Stream r -> Effect Unit -> Effect Unit
onErrorOrClose stream eff = do
  did <- Ref.new false
  let
    onE = do
      did' <- Ref.read did
      when (not did') (Ref.write true did *> eff)
  Event.once_ Stream.errorH (const onE) stream
  Event.once_ Stream.closeH onE stream
