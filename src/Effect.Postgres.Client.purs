module Effect.Postgres.Client where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Postgres (modifyPgTypes)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Foreign (Foreign, unsafeToForeign)
import Node.EventEmitter (EventHandle(..))
import Node.EventEmitter.UtilTypes (EventHandle1, EventHandle0)
import Prim.Row (class Union)
import Record (modify)
import Type.Prelude (Proxy(..))

foreign import data Client :: Type
foreign import makeImpl :: { unwrapMillis :: Milliseconds -> Number } -> Foreign -> Effect Client

type Notification =
  { processId :: Number
  , channel :: String
  , payload :: Maybe String
  }

type NotificationRaw =
  { processId :: Number
  , channel :: String
  , payload :: Nullable String
  }

type Config r =
  ( user :: String
  , password :: String
  , host :: String
  , port :: Number
  , database :: String
  , connectionString :: String
  , applicationName :: String
  , statementTimeout :: Milliseconds
  , queryTimeout :: Milliseconds
  , connectionTimeout :: Milliseconds
  , idleInTransactionTimeout :: Milliseconds
  | r
  )

make :: forall r trash. Union r trash (Config ()) => Record r -> Effect Client
make r = do
  modifyPgTypes
  makeImpl { unwrapMillis: unwrap } $ unsafeToForeign r

error :: EventHandle1 Client Error
error = EventHandle "end" mkEffectFn1

notice :: EventHandle1 Client Error
notice = EventHandle "notice" mkEffectFn1

end :: EventHandle0 Client
end = EventHandle "end" identity

notification :: EventHandle Client (Notification -> Effect Unit) (EffectFn1 NotificationRaw Unit)
notification =
  let
    payload = Proxy @"payload"
  in
    EventHandle "notification" (\f -> mkEffectFn1 $ f <<< modify payload Nullable.toMaybe)
