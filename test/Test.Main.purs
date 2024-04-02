module Test.Main where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Rec.Class (untilJust)
import Data.Either (Either(..), hush)
import Data.Filterable (filter)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Newtype (wrap)
import Data.String as String
import Effect (Effect, untilE)
import Effect.Aff (Aff, bracket, delay, launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Buffer as Buffer
import Node.ChildProcess (ChildProcess)
import Node.ChildProcess as ChildProcess
import Node.ChildProcess.Aff as ChildProcess.Aff
import Node.ChildProcess.Types (Exit(..), stringSignal)
import Node.Encoding (Encoding(..))
import Node.EventEmitter as Event
import Test.Control.Monad.Postgres as Test.Control.Monad.Postgres
import Test.Data.Postgres as Test.Data.Postgres
import Test.Data.Postgres.Custom as Test.Data.Postgres.Custom
import Test.Effect.Postgres.Client as Test.Effect.Postgres.Client
import Test.Effect.Postgres.Pool as Test.Effect.Postgres.Pool
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner (runSpec)

spawnDb :: Aff ChildProcess
spawnDb =
  let
    isReady = do
      { exitStatus, error } <- liftEffect (ChildProcess.spawnSync "docker" [ "compose", "exec", "db", "pg_isready" ])
      let
        exitOk (Normally 0) = true
        exitOk _ = false
      pure $ isNothing error && exitOk exitStatus
    waitReady =
      void $ untilJust do
        delay $ wrap $ 100.0
        isReady' <- isReady
        pure $ filter (const isReady') (Just unit)
  in
    do
      liftEffect $ log $ "[db] starting..."
      db <- liftEffect $ ChildProcess.spawn "docker" [ "compose", "up" ]
      waitReady
      liftEffect $ log $ "[db] started!"
      pure db

killDb :: ChildProcess -> Aff Unit
killDb db = do
  let
    onexit eff = Event.on ChildProcess.exitH eff db
    exit = makeAff \res -> mempty <$ onexit (const $ res $ Right unit)
  _ <- liftEffect $ ChildProcess.kill' (stringSignal "SIGTERM") db
  exit

main :: Effect Unit
main = launchAff_ do
  bracket spawnDb killDb
    $ const
    $ runSpec [ specReporter ] do
        Test.Data.Postgres.Custom.spec
        Test.Data.Postgres.spec
        Test.Effect.Postgres.Client.spec
        Test.Effect.Postgres.Pool.spec
        Test.Control.Monad.Postgres.spec
