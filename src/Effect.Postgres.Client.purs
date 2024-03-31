module Effect.Postgres.Client where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Postgres (modifyPgTypes)
import Data.Profunctor (lcmap)
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

-- | Database connection
foreign import data Client :: Type

-- | A notification raised by `NOTIFY`
type Notification =
  { processId :: Number
  , channel :: String
  , payload :: Maybe String
  }

-- | Client connection config
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

-- | Creates a new client, not yet connected to the database.
-- |
-- | The config parameter `r` is `Config` with all keys optional.
-- |
-- | <https://node-postgres.com/apis/client#new-client>
make :: forall r trash. Union r trash (Config ()) => Record r -> Effect Client
make r = do
  modifyPgTypes
  __make $ __uncfg { unwrapMillis: unwrap } $ unsafeToForeign r

-- | <https://node-postgres.com/apis/client#error>
errorE :: EventHandle1 Client Error
errorE = EventHandle "error" mkEffectFn1

-- | <https://node-postgres.com/apis/client#notice>
noticeE :: EventHandle1 Client Error
noticeE = EventHandle "notice" mkEffectFn1

-- | <https://node-postgres.com/apis/client#end>
endE :: EventHandle0 Client
endE = EventHandle "end" identity

-- | <https://node-postgres.com/apis/client#notification>
notificationE :: EventHandle Client (Notification -> Effect Unit) (EffectFn1 NotificationRaw Unit)
notificationE =
  let
    payload = Proxy @"payload"
    payloadToMaybe = modify payload Nullable.toMaybe
  in
    EventHandle "notification" (mkEffectFn1 <<< lcmap payloadToMaybe)

-- | FFI
foreign import data ClientConfigRaw :: Type

-- | FFI
foreign import __make :: ClientConfigRaw -> Effect Client

-- | FFI
foreign import __uncfg :: { unwrapMillis :: Milliseconds -> Number } -> Foreign -> ClientConfigRaw

-- | FFI
type NotificationRaw =
  { processId :: Number
  , channel :: String
  , payload :: Nullable String
  }
