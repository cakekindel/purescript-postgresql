module Pipes.Postgres where

import Prelude

import Control.Monad.Morph (hoist)
import Control.Monad.Postgres.Session (class MonadSession, streamIn, streamOut)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe)
import Effect.Aff.Class (liftAff)
import Node.Buffer (Buffer)
import Node.Stream.Object as O
import Pipes.Core (Consumer, Producer)
import Pipes.Node.Stream (fromReadable, fromWritable)

stdin :: forall m. MonadSession m => String -> Consumer (Maybe Buffer) m Unit
stdin q = do
  stream <- lift $ streamIn q
  hoist liftAff $ fromWritable $ O.fromBufferWritable stream

stdout :: forall m. MonadSession m => String -> Producer (Maybe Buffer) m Unit
stdout q = do
  stream <- lift $ streamOut q
  hoist liftAff $ fromReadable $ O.fromBufferReadable stream
