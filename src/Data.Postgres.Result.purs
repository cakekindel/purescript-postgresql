module Data.Postgres.Result where

import Prelude

import Control.Monad.Error.Class (liftMaybe, throwError)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Postgres (class Rep, RepT, deserialize)
import Data.Postgres.Raw (Raw)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import Foreign (ForeignError(..))
import Type.Prelude (Proxy(..))

foreign import data Result :: Type

foreign import rowsAffectedImpl :: Result -> Nullable Number
foreign import rows :: Result -> Array (Array Raw)

rowsAffected :: Result -> Maybe Int
rowsAffected = Int.fromNumber <=< Nullable.toMaybe <<< rowsAffectedImpl

class FromResult (a :: Type) where
  expectedRowLength :: forall g. g a -> Int
  fromRow :: Array Raw -> RepT a

instance (Rep a, FromResult b) => FromResult (a /\ b) where
  expectedRowLength _ = expectedRowLength (Proxy @b) + 1
  fromRow r =
    let
      expLen = expectedRowLength (Proxy @(Tuple a b))
      lengthMismatch = pure $ TypeMismatch ("Expected row of length " <> show expLen) ("Found row of length " <> show (Array.length r))
    in
      do
        when (Array.length r /= expLen) (throwError lengthMismatch)
        a <- deserialize =<< liftMaybe lengthMismatch (Array.head r)
        b <- fromRow =<< liftMaybe lengthMismatch (Array.tail r)
        pure $ a /\ b
else instance FromResult Unit where
  expectedRowLength _ = 0
  fromRow _ = pure unit
else instance Rep a => FromResult a where
  expectedRowLength _ = 1
  fromRow =
    let
      get [ a ] = pure a
      get o = throwError $ pure $ TypeMismatch "Expected row of length 1" $ show o
    in
      deserialize <=< get
