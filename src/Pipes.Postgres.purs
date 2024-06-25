module Pipes.Postgres where

import Prelude

import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Morph (hoist)
import Control.Monad.Postgres (PostgresT)
import Control.Monad.Reader (ask)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Postgres.Client as Client
import Effect.Aff.Postgres.Pool as Pool
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Postgres.Error.RE as RE
import Node.Buffer (Buffer)
import Node.Stream.Object as O
import Pipes ((>->))
import Pipes.Core (Consumer, Producer)
import Pipes.Node.Stream (fromReadable, fromWritable)
import Pipes.Prelude as Pipes

stdin
  :: forall m
   . MonadAff m
  => MonadError Error m
  => String
  -> Consumer (Maybe Buffer) (PostgresT m) Unit
stdin q = do
  pool <- lift ask
  client <- lift $ RE.liftExcept $ hoist liftAff $ Pool.connect pool
  stream <- lift $ RE.liftExcept $ hoist liftEffect $ Client.execWithStdin q client
  lift $ RE.liftExcept $ hoist liftAff $ void $ Client.exec "begin" client
  let
    releaseOnEOS Nothing = do
      RE.liftExcept $ hoist liftAff $ void $ Client.exec "commit" client
      RE.liftExcept $ hoist liftEffect $ Pool.release pool client
      pure Nothing
    releaseOnEOS (Just a) = pure (Just a)

    pipe = Pipes.mapM releaseOnEOS >-> hoist lift (fromWritable $ O.unsafeFromBufferWritable stream)
    err e = lift do
      RE.liftExcept $ hoist liftAff $ void $ Client.exec "rollback" client
      RE.liftExcept $ hoist liftEffect $ Pool.release pool client
      throwError e

  catchError pipe err

stdout
  :: forall m
   . MonadAff m
  => MonadThrow Error m
  => String
  -> Producer (Maybe Buffer) (PostgresT m) Unit
stdout q = do
  pool <- lift ask
  client <- lift $ RE.liftExcept $ hoist liftAff $ Pool.connect pool
  stream <- lift $ RE.liftExcept $ hoist liftEffect $ Client.queryWithStdout q client
  let
    releaseOnEOS :: Maybe Buffer -> PostgresT m (Maybe Buffer)
    releaseOnEOS Nothing = RE.liftExcept $ hoist liftEffect $ Pool.release pool client $> Nothing
    releaseOnEOS (Just a) = pure (Just a)
  hoist lift (fromReadable (O.unsafeFromBufferReadable stream))
    >-> Pipes.mapM releaseOnEOS
