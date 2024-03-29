module Test.Common where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Effect.Aff.Postgres.Client (connected, end)
import Effect.Class (liftEffect)
import Effect.Postgres.Client (Client)
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
withClient = bracket (connected =<< liftEffect config) end
