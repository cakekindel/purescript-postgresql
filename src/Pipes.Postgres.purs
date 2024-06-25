module Pipes.Postgres where

import Prelude

import Control.Monad.Cont (lift)
import Control.Monad.Morph (hoist)
import Control.Monad.Postgres (class MonadSession, streamIn, streamOut)
import Data.Maybe (Maybe)
import Effect.Aff.Class (liftAff)
import Node.Buffer (Buffer)
import Node.Stream.Object as O
import Pipes.Core (Consumer, Producer)
import Pipes.Node.Stream (fromReadable, fromWritable)

stdin :: forall m. MonadSession m => String -> Consumer (Maybe Buffer) m Unit
stdin q = do
  stream <- lift $ streamIn q
  hoist liftAff $ fromWritable $ O.unsafeFromBufferWritable stream

stdout :: forall m. MonadSession m => String -> Producer (Maybe Buffer) m Unit
stdout q = do
  stream <- lift $ streamOut q
  hoist liftAff $ fromReadable $ O.unsafeFromBufferReadable stream
