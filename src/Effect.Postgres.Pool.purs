module Effect.Postgres.Pool where

import Prelude

import Data.Newtype (unwrap)
import Data.Postgres (modifyPgTypes)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Postgres.Client (Client)
import Effect.Postgres.Client as Client
import Effect.Uncurried (EffectFn2, mkEffectFn1, mkEffectFn2)
import Foreign (Foreign, unsafeToForeign)
import Node.EventEmitter (EventHandle(..))
import Node.EventEmitter.UtilTypes (EventHandle1, EventHandle2)
import Prim.Row (class Union)
import Type.Row (type (+))

-- | Database connection pool
foreign import data Pool :: Type

-- | Pool construction config
-- |
-- | Includes all Client config options
type Config r = Client.Config + (idleTimeout :: Milliseconds, max :: Int, allowExitOnIdle :: Boolean | r)

-- | The total number of clients existing within the pool.
foreign import clientCount :: Pool -> Int

-- | The number of clients which are not checked out but are currently idle in the pool.
foreign import clientIdleCount :: Pool -> Int

-- | The number of queued requests waiting on a client when all clients are checked out.
-- | It can be helpful to monitor this number to see if you need to adjust the size of the pool.
foreign import clientWaitingCount :: Pool -> Int

-- | Creates a new pool
-- |
-- | The config parameter `r` is `Config` with all keys optional.
-- |
-- | <https://node-postgres.com/apis/pool#new-pool>
make :: forall @r @missing @trash. Union r missing (Config trash) => Record r -> Effect Pool
make r = do
  modifyPgTypes
  let asClientConfig = Client.__uncfg { unwrapMillis: unwrap } $ unsafeToForeign r
  __make $ __uncfg { unwrapMillis: unwrap } $ unsafeToForeign asClientConfig

-- | <https://node-postgres.com/apis/pool#releasing-clients>
release :: Pool -> Client -> Effect Unit
release p c = __release p c false

-- | <https://node-postgres.com/apis/pool#releasing-clients>
destroy :: Pool -> Client -> Effect Unit
destroy p c = __release p c true

-- | <https://node-postgres.com/apis/pool#connect>
connectE :: EventHandle1 Pool Client
connectE = EventHandle "connect" mkEffectFn1

-- | <https://node-postgres.com/apis/pool#error>
errorE :: EventHandle2 Pool Error Client
errorE = EventHandle "error" mkEffectFn2

-- | <https://node-postgres.com/apis/pool#acquire>
acquireE :: EventHandle1 Pool Client
acquireE = EventHandle "acquire" mkEffectFn1

-- | <https://node-postgres.com/apis/pool#remove>
removeE :: EventHandle1 Pool Client
removeE = EventHandle "remove" mkEffectFn1

-- | <https://node-postgres.com/apis/pool#release>
releaseE :: EventHandle Pool (Client -> Effect Unit) (EffectFn2 Foreign Client Unit)
releaseE = EventHandle "release" (mkEffectFn2 <<< const)

-- | FFI type for `import('pg').PoolConfig`
foreign import data PoolConfigRaw :: Type

-- | FFI binding to `new Pool()`
foreign import __make :: PoolConfigRaw -> Effect Pool

-- | FFI binding to `Client#release` for clients created from pools
-- |
-- | Accepts a `Pool` as a type-level hint that the `Client` should have
-- | come from a `Pool`
foreign import __release :: Pool -> Client -> Boolean -> Effect Unit

-- | partial `Config` to `import('pg').PoolConfig`
foreign import __uncfg :: { unwrapMillis :: Milliseconds -> Number } -> Foreign -> PoolConfigRaw
