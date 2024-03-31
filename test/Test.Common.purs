module Test.Common where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, Fiber, bracket, forkAff, joinFiber, launchAff)
import Effect.Aff.Postgres.Client (Client)
import Effect.Aff.Postgres.Client as Client
import Effect.Aff.Postgres.Pool (Pool)
import Effect.Aff.Postgres.Pool as Pool
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Node.Path as Path
import Node.Process (cwd)

config
  :: Effect
       { database :: String
       , host :: String
       , password :: String
       , user :: String
       }
config = do
  cwd' <- liftEffect cwd
  host <- liftEffect $ Path.resolve [ cwd' ] "./pg"
  pure { host, user: "postgres", password: "password", database: "postgres" }

withClient :: (Client -> Aff Unit) -> Aff Unit
withClient = bracket (Client.connected =<< liftEffect config) Client.end

pool :: Pool
pool = unsafePerformEffect $ Pool.make =<< liftEffect config

withPoolClient :: (Client -> Aff Unit) -> Aff Unit
withPoolClient = bracket (Pool.connect pool) (liftEffect <<< Pool.releaseClient pool)
