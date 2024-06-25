module Pipes.Postgres where

import Prelude

import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Morph (hoist)
import Control.Monad.Postgres (SessionT)
import Control.Monad.Reader (ask)
import Data.Maybe (Maybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Postgres.Client as Client
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Postgres.Error.RE as RE
import Node.Buffer (Buffer)
import Node.Stream.Object as O
import Pipes.Core (Consumer, Producer)
import Pipes.Node.Stream (fromReadable, fromWritable)

stdin
  :: forall m
   . MonadAff m
  => MonadError Error m
  => String
  -> Consumer (Maybe Buffer) (SessionT m) Unit
stdin q = do
  client <- lift ask
  stream <- lift $ RE.liftExcept $ hoist liftEffect $ Client.execWithStdin q client
  hoist lift (fromWritable $ O.unsafeFromBufferWritable stream)

stdout
  :: forall m
   . MonadAff m
  => MonadThrow Error m
  => String
  -> Producer (Maybe Buffer) (SessionT m) Unit
stdout q = do
  client <- lift ask
  stream <- lift $ RE.liftExcept $ hoist liftEffect $ Client.queryWithStdout q client
  hoist lift (fromReadable (O.unsafeFromBufferReadable stream))
