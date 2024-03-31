module Effect.Aff.Postgres.Pool (connect, end, __end, __connect, module X) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Postgres.Client (Client)
import Effect.Postgres.Pool (Pool)
import Effect.Postgres.Pool as X

-- | Acquires a client from the pool.
-- | 
-- |  * If there are idle clients in the pool one will be returned to the callback on process.nextTick.
-- |  * If the pool is not full but all current clients are checked out a new client will be created & returned to this callback.
-- |  * If the pool is 'full' and all clients are currently checked out will wait in a FIFO queue until a client becomes available by it being released back to the pool.
-- |
-- | <https://node-postgres.com/apis/pool#poolconnect>
connect :: Pool -> Aff Client
connect = Promise.toAffE <<< __connect

-- | Drain the pool of all active clients, disconnect them,
-- | and shut down any internal timers in the pool.
-- |
-- | It is common to call this at the end of a script using the pool or when
-- | your process is attempting to shut down cleanly.
-- |
-- | <https://node-postgres.com/apis/pool#poolend>
end :: Pool -> Aff Unit
end = Promise.toAffE <<< __end

-- | FFI binding to `Pool#end`
foreign import __end :: Pool -> Effect (Promise Unit)

-- | FFI binding to `Pool#connect`
foreign import __connect :: Pool -> Effect (Promise Client)
