module Effect.Postgres.Error.Except where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, liftEither, try)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Morph (hoist)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Postgres (RepT)
import Data.Postgres.Query (Query)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception as Effect
import Effect.Postgres.Error.Common (E, Error(..), toException)

type Except m = ExceptT E m

run :: forall m a. MonadThrow Effect.Error m => Except m a -> m a
run m = liftEither =<< lmap toException <$> runExceptT m

toEither :: forall m a. Except m a -> m (Either E a)
toEither = runExceptT

with :: forall e m a. MonadError e m => (e -> Error) -> m a -> Except m a
with e m = ExceptT $ map (lmap $ pure <<< e) $ try m

withEither :: forall e m a. Monad m => (e -> Error) -> m (Either e a) -> Except m a
withEither e m = ExceptT $ map (lmap $ pure <<< e) $ m

exception :: forall m a. MonadError Effect.Error m => m a -> Except m a
exception = with Other

executing :: forall m a. MonadError Effect.Error m => Query -> m a -> Except m a
executing q = with (Executing q)

parsing :: forall m a. MonadEffect m => Query -> RepT a -> Except m a
parsing q m = do
  e <- hoist liftEffect $ exception $ runExceptT m
  withEither (Deserializing q) (pure e)

printing :: forall m a. MonadEffect m => RepT a -> Except m a
printing m = do
  e <- hoist liftEffect $ exception $ runExceptT m
  withEither Serializing (pure e)
