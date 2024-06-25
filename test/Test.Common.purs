module Test.Common where

import Prelude

import Data.Either (Either(..))
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (RegexFlags)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, bracket, makeAff)
import Effect.Aff.Postgres.Client (Client)
import Effect.Aff.Postgres.Client as Client
import Effect.Aff.Postgres.Pool (Pool)
import Effect.Aff.Postgres.Pool as Pool
import Effect.Class (liftEffect)
import Effect.Postgres.Error.Except as X
import Effect.Unsafe (unsafePerformEffect)
import Node.EventEmitter (EventHandle)
import Node.EventEmitter as EE
import Node.Path as Path
import Node.Process (cwd)
import Partial.Unsafe (unsafePartial)

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
withClient = bracket (X.run $ Client.connected =<< liftEffect config) (X.run <<< Client.end)

pool :: Pool
pool = unsafePerformEffect $ Pool.make =<< liftEffect config

withPool :: (Pool -> Aff Unit) -> Aff Unit
withPool = bracket (liftEffect $ Pool.make =<< config) (X.run <<< Pool.end)

withPoolClient :: (Client -> Aff Unit) -> Aff Unit
withPoolClient = bracket (X.run $ Pool.connect pool) (liftEffect <<< X.run <<< Pool.release pool)

unsafeFromRight :: forall a b. Either a b -> b
unsafeFromRight e = unsafePartial $ case e of Right b -> b

re :: String -> RegexFlags -> Regex
re s f = unsafeFromRight $ Regex.regex s f

class Curried a b | a -> b where
  curried :: (b -> Effect Unit) -> a

instance Curried (Effect Unit) Unit where
  curried f = f unit

instance Curried (a -> Effect Unit) a where
  curried = identity

instance Curried (a -> b -> Effect Unit) (a /\ b) where
  curried f = \a b -> f $ a /\ b

instance Curried (a -> b -> c -> Effect Unit) (a /\ b /\ c) where
  curried f = \a b c -> f $ a /\ b /\ c

onceAff :: forall psCb jsCb emitter r. Curried psCb r => EventHandle emitter psCb jsCb -> emitter -> Aff r
onceAff ev em = makeAff \res -> mempty <* EE.once ev (curried (res <<< Right)) em
