module Pipes.Postgres where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Postgres (class MonadPostgres)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Postgres.Client as Client
import Effect.Aff.Postgres.Pool (Pool)
import Effect.Aff.Postgres.Pool as Pool
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Node.Buffer (Buffer)
import Node.Stream.Object as O
import Pipes ((>->))
import Pipes.Core (Consumer, Producer)
import Pipes.Node.Stream (fromReadable, fromWritable)
import Pipes.Prelude as Pipes

stdin
  :: forall m s c ct
   . MonadAff m
  => MonadError Error m
  => MonadAsk Pool m
  => MonadPostgres m s c ct
  => String
  -> Consumer (Maybe Buffer) m Unit
stdin q = do
  pool <- ask
  client <- liftAff $ Pool.connect pool
  stream <- liftEffect $ Client.execWithStdin q client
  liftAff $ void $ Client.exec "begin" client
  let
    releaseOnEOS Nothing = do
      liftAff $ void $ Client.exec "commit" client
      liftEffect $ Pool.release pool client
      pure Nothing
    releaseOnEOS (Just a) = pure (Just a)

    pipe = Pipes.mapM releaseOnEOS >-> fromWritable (O.unsafeFromBufferWritable stream)
    err e = do
      liftAff $ void $ Client.exec "rollback" client
      liftEffect $ Pool.release pool client
      throwError e

  catchError pipe err

stdout
  :: forall m s c ct
   . MonadAff m
  => MonadThrow Error m
  => MonadAsk Pool m
  => MonadPostgres m s c ct
  => String
  -> Producer (Maybe Buffer) m Unit
stdout q = do
  pool <- ask
  client <- liftAff $ Pool.connect pool
  stream <- liftEffect $ Client.queryWithStdout q client
  let
    releaseOnEOS Nothing = liftEffect $ Pool.release pool client $> Nothing
    releaseOnEOS (Just a) = pure (Just a)
  fromReadable (O.unsafeFromBufferReadable stream) >-> Pipes.mapM releaseOnEOS
