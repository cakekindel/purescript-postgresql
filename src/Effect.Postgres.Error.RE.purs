module Effect.Postgres.Error.RE where

import Prelude hiding (join)

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, class Plus)
import Control.Monad.Base (class MonadBase)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, liftEither, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Fork.Class (class MonadBracket, class MonadFork, class MonadKill, BracketCondition, bracket, fork, join, kill, never, suspend, uninterruptible)
import Control.Monad.Fork.Class as Bracket
import Control.Monad.Morph (class MFunctor, class MMonad, embed, hoist)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT(..), runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Unlift (class MonadUnlift, withRunInBase)
import Control.Parallel (class Parallel, parallel, sequential)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Functor.Compose (Compose)
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Unlift (class MonadUnliftAff, withRunInAff)
import Effect.Class (class MonadEffect)
import Effect.Exception as Effect
import Effect.Postgres.Error.Common (E, Error(..), toException)
import Effect.Postgres.Error.Except (Except)
import Effect.Unlift (class MonadUnliftEffect, withRunInEffect)

-- | `ReaderT` with `ExceptT E`
-- |
-- | `ReaderT r (ExceptT (NonEmptyList Effect.Postgres.Error.Error) m) a`
newtype RE r m a = RE (ReaderT r (ExceptT E m) a)

newtype ParRE r f a = ParRE (ReaderT r (Compose f (Either E)) a)

finally :: forall r m a. Monad m => RE r m Unit -> RE r m a -> RE r m a
finally after m = (m <* after) `catchError` \e -> after *> throwError e

run :: forall m r a. MonadThrow Effect.Error m => RE r m a -> r -> m a
run m r = liftEither =<< lmap toException <$> runExceptT (runReaderT (unwrap m) r)

toExcept :: forall m r a. RE r m a -> r -> Except m a
toExcept m r = runReaderT (unwrap m) r

toEither :: forall m r a. RE r m a -> r -> m (Either E a)
toEither m r = runExceptT $ runReaderT (unwrap m) r

liftExcept :: forall m r a. Monad m => Except m a -> RE r m a
liftExcept = wrap <<< lift

derive instance Newtype (ParRE r m a) _
derive newtype instance Functor m => Functor (ParRE r m)
derive newtype instance Apply m => Apply (ParRE r m)
derive newtype instance Applicative m => Applicative (ParRE r m)
derive newtype instance Alt m => Alt (ParRE r m)
derive newtype instance Plus m => Plus (ParRE r m)
derive newtype instance Alternative m => Alternative (ParRE r m)

derive instance Newtype (RE r m a) _
derive newtype instance Monad m => MonadAsk r (RE r m)
derive newtype instance Monad m => MonadReader r (RE r m)
derive newtype instance Monad m => Functor (RE r m)
derive newtype instance Monad m => Apply (RE r m)
derive newtype instance Monad m => Applicative (RE r m)
derive newtype instance Monad m => Bind (RE r m)
derive newtype instance Monad m => Monad (RE r m)
derive newtype instance Monad m => MonadError E (RE r m)
derive newtype instance Monad m => MonadThrow E (RE r m)
derive newtype instance MonadEffect m => MonadEffect (RE r m)
derive newtype instance MonadAff m => MonadAff (RE r m)
derive newtype instance MonadRec m => MonadRec (RE r m)

instance (Monad m, Parallel p m) => Parallel (ParRE r p) (RE r m) where
  parallel = wrap <<< parallel <<< unwrap
  sequential = wrap <<< sequential <<< unwrap

instance MonadTrans (RE r) where
  lift = wrap <<< lift <<< lift

instance MFunctor (RE r) where
  hoist mn (RE m) = RE $ hoist (hoist mn) m

instance MMonad (RE r) where
  embed :: forall n m b. Monad n => (forall a. m a -> RE r n a) -> RE r m b -> RE r n b
  embed mtn (RE m) =
    RE $ ReaderT $ \r -> embed (flip runReaderT r <<< unwrap <<< mtn) (runReaderT m r)

instance Monad m => Alt (RE r m) where
  alt a b = catchError b (\e -> catchError a (\e' -> throwError $ e <> e'))

instance (MonadThrow Effect.Error m, MonadUnliftEffect m) => MonadUnliftEffect (RE r m) where
  withRunInEffect f = RE $ ReaderT $ \r -> lift $ withRunInEffect @m $ \a -> f (a <<< flip run r)

instance (MonadThrow Effect.Error m, MonadUnliftAff m) => MonadUnliftAff (RE r m) where
  withRunInAff f = RE $ ReaderT $ \r -> lift $ withRunInAff @m $ \a -> f (a <<< flip run r)

instance (MonadBase m (RE r m), MonadThrow Effect.Error m) => MonadUnlift m (RE r m) where
  withRunInBase f = RE $ ReaderT $ \r -> lift $ f (flip run r)

instance Monad m => MonadBase m (RE r m) where
  liftBase = lift

instance (MonadThrow Effect.Error m, MonadFork f m) => MonadFork f (RE r m) where
  fork m = withRunInBase \f -> fork $ f m
  suspend m = withRunInBase \f -> suspend $ f m
  join f = lift $ join f

instance (MonadKill Effect.Error f m) => MonadKill E f (RE r m) where
  kill e f = lift $ kill (toException e) f

instance (MonadBracket Effect.Error f m) => MonadBracket E f (RE r m) where
  bracket acq rel m = withRunInBase \f -> bracket (f acq) (\c r -> f $ rel ((bracketCondError (pure <<< Other)) c) r) (f <<< m)
  uninterruptible = hoist uninterruptible
  never = lift never

bracketCondError :: forall ea eb a. (ea -> eb) -> BracketCondition ea a -> BracketCondition eb a
bracketCondError _ (Bracket.Completed a) = Bracket.Completed a
bracketCondError f (Bracket.Failed a) = Bracket.Failed $ f a
bracketCondError f (Bracket.Killed a) = Bracket.Killed $ f a
