module Pipes.Postgres where

import Prelude

import Control.Monad.Cont (lift)
import Control.Monad.Morph (hoist)
import Control.Monad.Postgres (SessionT)
import Control.Monad.Reader (ask)
import Data.Maybe (Maybe)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Postgres.Client as Client
import Effect.Class (liftEffect)
import Effect.Postgres.Error.RE as RE
import Node.Buffer (Buffer)
import Node.Stream.Object as Node.Stream.Object
import Pipes.Core (Consumer, Producer)
import Pipes.Node.Stream as Pipe.Node

stdin :: forall m. MonadAff m => String -> Consumer (Maybe Buffer) (SessionT m) Unit
stdin q = do
  client <- lift ask
  stream <- lift $ RE.liftExcept $ hoist liftEffect $ Client.execWithStdin q client
  hoist liftAff $ Pipe.Node.fromWritable $ Node.Stream.Object.unsafeFromBufferWritable stream

stdout :: forall m. MonadAff m => String -> Producer (Maybe Buffer) (SessionT m) Unit
stdout q = do
  client <- lift ask
  stream <- lift $ RE.liftExcept $ hoist liftEffect $ Client.queryWithStdout q client
  hoist liftAff $ Pipe.Node.fromReadable $ Node.Stream.Object.unsafeFromBufferReadable stream
