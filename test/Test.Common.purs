module Test.Common where

import Prelude

import Data.Either (Either(..))
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (RegexFlags(..))
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Effect.Aff.Postgres.Client (Client)
import Effect.Aff.Postgres.Client as Client
import Effect.Aff.Postgres.Pool (Pool)
import Effect.Aff.Postgres.Pool as Pool
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Node.Path as Path
import Node.Process (cwd)
import Partial.Unsafe (unsafePartial)
import Record (insert)
import Type.Prelude (Proxy(..))

type Config =
  { database :: String
  , host :: String
  , password :: String
  , user :: String
  , max :: Int
  }

config :: Effect Config
config = do
  cwd' <- liftEffect cwd
  host <- liftEffect $ Path.resolve [ cwd' ] "./pg"
  pure { host, user: "postgres", password: "password", database: "postgres", max: 3 }

withConfig :: (Config -> Aff Unit) -> Aff Unit
withConfig f = f =<< liftEffect config

withClient :: (Client -> Aff Unit) -> Aff Unit
withClient = bracket (Client.connected =<< liftEffect config) Client.end

pool :: Pool
pool = unsafePerformEffect $ Pool.make =<< liftEffect config

withPool :: (Pool -> Aff Unit) -> Aff Unit
withPool = bracket (liftEffect $ Pool.make =<< config) Pool.end

withPoolClient :: (Client -> Aff Unit) -> Aff Unit
withPoolClient = bracket (Pool.connect pool) (liftEffect <<< Pool.release pool)

unsafeFromRight :: forall a b. Either a b -> b
unsafeFromRight e = unsafePartial $ case e of Right b -> b

re :: String -> RegexFlags -> Regex
re s f = unsafeFromRight $ Regex.regex s f
