module Effect.Postgres.Error.RE where

import Prelude hiding (join)

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, class Plus)
import Control.Monad.Base (class MonadBase)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, liftEither, liftMaybe, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Fork.Class (class MonadBracket, class MonadFork, class MonadKill, bracket, fork, join, kill, never, suspend, uninterruptible)
import Control.Monad.Fork.Class as Bracket
import Control.Monad.Morph (class MFunctor, class MMonad, embed, hoist)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT(..), ask, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Unlift (class MonadUnlift)
import Control.Parallel (class Parallel, parallel, sequential)
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Bifunctor (lmap)
import Data.Either (Either, blush, hush)
import Data.Functor.Compose (Compose(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (for_, traverse_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Unlift (class MonadUnliftAff, withRunInAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception as Effect
import Effect.Exception as Exception
import Effect.Postgres.Error.Common (E, Error(..), toException)
import Effect.Postgres.Error.Except (Except)
import Effect.Postgres.Error.Except as E
import Effect.Ref as Ref
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
derive newtype instance MonadRec m => MonadRec (RE r m)

instance MonadEffect m => MonadEffect (RE r m) where
  liftEffect m = hoist liftEffect $ liftExcept $ E.exception m

instance MonadAff m => MonadAff (RE r m) where
  liftAff m = hoist liftAff $ liftExcept $ E.exception m

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

instance (MonadThrow Effect.Error m, MonadFork f m) => MonadFork (Compose f (Either E)) (RE r m) where
  fork m = RE $ ReaderT \r -> lift $ Compose <$> fork (toEither m r)
  suspend m = RE $ ReaderT \r -> lift $ Compose <$> suspend (toEither m r)
  join f = liftEither =<< lift (join $ unwrap f)

instance (MonadKill Effect.Error f m) => MonadKill E (Compose f (Either E)) (RE r m) where
  kill e f = lift $ kill (toException e) (unwrap f)

instance (MonadEffect m, MonadBracket Effect.Error f m) => MonadBracket E (Compose f (Either E)) (RE r m) where
  bracket acq rel go = do
    r <- ask

    errs <- liftEffect $ Ref.new []

    let
      eErrsEmpty = pure $ Other $ Exception.error "no errors"
      appendErrs = liftEffect <<< flip Ref.modify_ errs <<< (<>) <<< Array.NonEmpty.toArray
      readErrs = liftEffect $ Array.NonEmpty.fromArray <$> Ref.read errs

      run' :: forall a. RE r m a -> m (Maybe a)
      run' m = do
        either <- toEither m r
        traverse_ appendErrs $ blush either
        pure $ hush either

      rel' _ Nothing = pure unit
      rel' (Bracket.Failed e) (Just a) = void $ run' $ rel (Bracket.Failed $ pure $ Other e) a
      rel' (Bracket.Killed e) (Just a) = void $ run' $ rel (Bracket.Killed $ pure $ Other e) a
      rel' (Bracket.Completed (Just ret)) (Just a) = void $ run' $ rel (Bracket.Completed ret) a
      rel' (Bracket.Completed Nothing) (Just a) = void $ run' do
        errs' <- fromMaybe eErrsEmpty <$> readErrs
        rel (Bracket.Failed errs') a

      acq' = run' acq

      go' (Just a) = run' $ go a
      go' Nothing = pure Nothing

    ret <- lift $ bracket acq' rel' go'
    errs' <- readErrs
    for_ errs' throwError
    liftMaybe eErrsEmpty ret
  uninterruptible = hoist uninterruptible
  never = lift never
