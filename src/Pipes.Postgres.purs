module Pipes.Postgres where

import Prelude

import Control.Monad.Morph (hoist)
import Control.Monad.Postgres (class MonadPostgres)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Postgres.Client as Client
import Effect.Aff.Postgres.Pool (Pool)
import Effect.Aff.Postgres.Pool as Pool
import Effect.Class (liftEffect)
import Node.Buffer (Buffer)
import Node.Stream.Object as O
import Pipes ((>->))
import Pipes.Core (Consumer, Producer)
import Pipes.Node.Stream (fromReadable, fromWritable)
import Pipes.Prelude as Pipes

stdin
  :: forall m s c ct
   . MonadAff m
  => MonadAsk Pool m
  => MonadPostgres m s c ct
  => String
  -> Consumer (Maybe Buffer) m Unit
stdin q = do
  pool <- ask
  client <- liftAff $ Pool.connect pool
  stream <- liftEffect $ Client.execWithStdin q client
  let
    releaseOnEOS Nothing = Pool.release pool client $> Nothing
    releaseOnEOS (Just a) = pure (Just a)
  hoist liftAff
    $ hoist liftEffect (Pipes.mapM releaseOnEOS)
        >-> fromWritable (O.fromBufferWritable stream)

stdout
  :: forall m s c ct
   . MonadAff m
  => MonadAsk Pool m
  => MonadPostgres m s c ct
  => String
  -> Producer (Maybe Buffer) m Unit
stdout q = do
  pool <- ask
  client <- liftAff $ Pool.connect pool
  stream <- liftEffect $ Client.queryWithStdout q client
  let
    releaseOnEOS Nothing = Pool.release pool client $> Nothing
    releaseOnEOS (Just a) = pure (Just a)
  hoist liftAff
    $ fromReadable (O.fromBufferReadable stream)
        >-> hoist liftEffect (Pipes.mapM releaseOnEOS)
