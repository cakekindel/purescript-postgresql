module Control.Monad.Postgres.Cursor where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, class Plus)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Fork.Class (class MonadBracket, class MonadFork, class MonadKill, bracket, kill, never, uninterruptible)
import Control.Monad.Postgres.Session (class MonadSession, exec, exec_, query, streamIn, streamOut)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, local)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Parallel (class Parallel, parallel, sequential)
import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Postgres (RepT, smash)
import Data.Postgres.Raw (Raw)
import Data.Postgres.Result (RowsAffected(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Unlift (class MonadUnliftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Postgres.Error as E
import Effect.Unlift (class MonadUnliftEffect)

data Move
  -- | `MOVE RELATIVE`
  = MoveRelative Int
  -- | `MOVE ABSOLUTE`
  | MoveTo Int

newtype CursorT :: forall k. Type -> (k -> Type) -> k -> Type
newtype CursorT t m a = CursorT (ReaderT (String /\ (Array Raw -> RepT t)) m a)

derive instance Newtype (CursorT t m a) _
derive newtype instance (Functor m) => Functor (CursorT t m)
derive newtype instance (Apply m) => Apply (CursorT t m)
derive newtype instance (Applicative m) => Applicative (CursorT t m)
derive newtype instance (Plus m) => Plus (CursorT t m)
derive newtype instance (Alt m) => Alt (CursorT t m)
derive newtype instance (Alternative m) => Alternative (CursorT t m)
derive newtype instance (Bind m) => Bind (CursorT t m)
derive newtype instance (Monad m) => Monad (CursorT t m)
derive newtype instance (MonadEffect m) => MonadEffect (CursorT t m)
derive newtype instance (MonadAff m) => MonadAff (CursorT t m)
derive newtype instance MonadRec m => MonadRec (CursorT t m)
derive newtype instance (MonadUnliftEffect m) => MonadUnliftEffect (CursorT t m)
derive newtype instance (MonadUnliftAff m) => MonadUnliftAff (CursorT t m)
derive newtype instance MonadTrans (CursorT t)
derive newtype instance (MonadThrow e m) => MonadThrow e (CursorT t m)
derive newtype instance (MonadError e m) => MonadError e (CursorT t m)
derive newtype instance (MonadFork f m) => MonadFork f (CursorT t m)
instance (Monad m, MonadKill e f m) => MonadKill e f (CursorT t m) where
  kill a b = lift $ kill a b

instance (Monad m, MonadBracket e f (ReaderT String m), MonadBracket e f m) => MonadBracket e f (CursorT t m) where
  bracket acq rel m = wrap $ bracket (unwrap acq) (\a b -> unwrap $ rel a b) (unwrap <<< m)
  uninterruptible a = wrap $ uninterruptible $ unwrap a
  never = lift $ never

instance Monad m => MonadAsk (String /\ (Array Raw -> RepT t)) (CursorT t m) where
  ask = wrap ask

instance Monad m => MonadReader (String /\ (Array Raw -> RepT t)) (CursorT t m) where
  local f m = wrap $ local f $ unwrap m

instance (Apply m, Apply p, Parallel p m) => Parallel (CursorT t p) (CursorT t m) where
  parallel = wrap <<< parallel <<< unwrap
  sequential = wrap <<< sequential <<< unwrap

-- | A monad representing a handle to a server-side cursor
-- |
-- | ```
-- | runPostgres {connectionString: "..."} do
-- |   exec_ "create table foo (id int not null primary key);"
-- |   exec_
-- |     $ intercalate "\n "
-- |       [ "insert into foo (id)"
-- |       , "values"
-- |       , intercalate ", "
-- |           $ map (\n -> "(" <> show n <> ")")
-- |           $ Array.range 1 100
-- |       ]
-- |
-- |   cursor @Int "foo_cursor" "select id from foo" do
-- |     a <- fetchOne -- 1
-- |     b <- fetchOne -- 2
-- |     c <- fetchOne -- 3
-- |     d <- fetch 10 -- 4..14
-- |     e <- fetchAll -- 15..100
-- |     pure unit
-- | ```
class (MonadSession m) <= MonadCursor m t where
  -- | Fetch a specified number of rows from the cursor
  fetch :: Int -> m (Array t)
  -- | Fetch all remaining rows from the cursor
  fetchAll :: m (Array t)
  -- | Change the cursor's position without fetching any data,
  -- | returning the number of rows skipped.
  move :: Move -> m Int

instance (MonadSession m) => MonadCursor (CursorT t m) t where
  fetch n = do
    cur /\ f <- ask
    raw :: Array (Array Raw) <- query $ "fetch forward " <> show n <> " from " <> cur
    liftEffect $ smash $ traverse f raw
  fetchAll = do
    cur /\ f <- ask
    raw :: Array (Array Raw) <- query $ "fetch all from " <> cur
    liftEffect $ smash $ traverse f raw
  move (MoveTo n) = do
    cur /\ _ <- ask
    RowsAffected n' <- query $ ("move absolute $1 from " <> cur) /\ n
    pure n'
  move (MoveRelative n) = do
    cur /\ _ <- ask
    RowsAffected n' <- query $ ("move relative $1 from " <> cur) /\ n
    pure n'

instance (MonadThrow E.E m, MonadSession m) => MonadSession (CursorT t m) where
  query = lift <<< query
  exec = lift <<< exec
  exec_ = lift <<< exec_
  streamIn = lift <<< streamIn
  streamOut = lift <<< streamOut

-- | Fetch the next row from the cursor
fetchOne :: forall m t. MonadCursor m t => m (Maybe t)
fetchOne = Array.head <$> fetch 1
