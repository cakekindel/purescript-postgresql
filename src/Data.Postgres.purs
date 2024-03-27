module Data.Postgres where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftEither, liftMaybe)
import Control.Monad.Except (Except, ExceptT, except, runExcept, runExceptT, withExceptT)
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Postgres.Raw (Raw)
import Data.Postgres.Raw (unsafeFromForeign, unsafeToForeign) as Raw
import Data.RFC3339String as DateTime.ISO
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Exception (error)
import Foreign (ForeignError)
import Foreign as F
import Node.Buffer (Buffer)

newtype JSON = JSON String

foreign import null_ :: Raw

-- | Important! This effect MUST be evaluated to guarantee
-- | that (de)serialization will work for timestamp and JSON types.
-- |
-- | This mutates the `pg-types`, overriding the default deserialization
-- | behavior for JSON and timestamp types.
foreign import modifyPgTypes :: Effect Unit

-- | The SQL value NULL
data Null = Null

derive instance Generic Null _
derive instance Eq Null
derive instance Ord Null
instance Show Null where
  show = genericShow

-- | The serialization & deserialization monad.
type RepT a = ExceptT RepError Effect a

-- | Errors encounterable while serializing & deserializing.
data RepError
  = RepErrorTypeMismatch { expected :: String, found :: String }
  | RepErrorInvalid String
  | RepErrorForeign ForeignError
  | RepErrorOther String
  | RepErrorMultiple (NonEmptyList RepError)

derive instance Generic RepError _
derive instance Eq RepError
instance Show RepError where
  show a = genericShow a

instance Semigroup RepError where
  append (RepErrorMultiple as) (RepErrorMultiple bs) = RepErrorMultiple (as <> bs)
  append (RepErrorMultiple as) b = RepErrorMultiple (as <> pure b)
  append a (RepErrorMultiple bs) = RepErrorMultiple (pure a <> bs)
  append a b = RepErrorMultiple (pure a <> pure b)

-- | Flatten to an Effect, rendering any `RepError`s to `String` using `Show`.
smash :: forall a. RepT a -> Effect a
smash = liftEither <=< map (lmap (error <<< show)) <<< runExceptT

-- | Lift an `Except` returned by functions in the `Foreign` module to `RepT`
liftForeign :: forall a. Except (NonEmptyList ForeignError) a -> RepT a
liftForeign = except <<< runExcept <<< withExceptT (RepErrorMultiple <<< map RepErrorForeign)

-- | Serialize data of type `a` to a `Raw` SQL value.
class Serialize a where
  serialize :: a -> RepT Raw

-- | Deserialize data of type `a` from a `Raw` SQL value.
class Deserialize a where
  deserialize :: Raw -> RepT a

-- | A type which is `Rep`resentable as a SQL value.
class (Serialize a, Deserialize a) <= Rep a

instance (Serialize a, Deserialize a) => Rep a

-- | Coerces the value to `Raw`
unsafeSerializeCoerce :: forall m a. Monad m => a -> m Raw
unsafeSerializeCoerce = pure <<< Raw.unsafeFromForeign <<< F.unsafeToForeign

instance Serialize Raw where
  serialize = pure

instance Serialize Null where
  serialize _ = unsafeSerializeCoerce null_

-- | `bytea`
instance Serialize Buffer where
  serialize = unsafeSerializeCoerce

instance Serialize Int where
  serialize = unsafeSerializeCoerce

instance Serialize Boolean where
  serialize = unsafeSerializeCoerce

instance Serialize String where
  serialize = unsafeSerializeCoerce

instance Serialize Number where
  serialize = unsafeSerializeCoerce

instance Serialize Char where
  serialize = unsafeSerializeCoerce

instance Serialize DateTime where
  serialize = serialize <<< unwrap <<< DateTime.ISO.fromDateTime

instance Serialize a => Serialize (Maybe a) where
  serialize (Just a) = serialize a
  serialize Nothing = unsafeSerializeCoerce null_

instance Serialize a => Serialize (Array a) where
  serialize = unsafeSerializeCoerce <=< traverse serialize

instance Deserialize Raw where
  deserialize = pure

-- | `bytea`
instance Deserialize Buffer where
  deserialize = liftForeign <<< (F.unsafeReadTagged "Buffer") <<< Raw.unsafeToForeign

instance Deserialize Null where
  deserialize = map (const Null) <<< liftForeign <<< F.readNullOrUndefined <<< Raw.unsafeToForeign

instance Deserialize Int where
  deserialize = liftForeign <<< F.readInt <<< Raw.unsafeToForeign

instance Deserialize Boolean where
  deserialize = liftForeign <<< F.readBoolean <<< Raw.unsafeToForeign

instance Deserialize String where
  deserialize = liftForeign <<< F.readString <<< Raw.unsafeToForeign

instance Deserialize Number where
  deserialize = liftForeign <<< F.readNumber <<< Raw.unsafeToForeign

instance Deserialize Char where
  deserialize = liftForeign <<< F.readChar <<< Raw.unsafeToForeign

instance Deserialize DateTime where
  deserialize raw = do
    s :: String <- deserialize raw
    let invalid = RepErrorInvalid $ "Not a valid ISO8601 string: `" <> s <> "`"
    liftMaybe invalid $ DateTime.ISO.toDateTime $ wrap s

instance Deserialize a => Deserialize (Array a) where
  deserialize = traverse (deserialize <<< Raw.unsafeFromForeign) <=< liftForeign <<< F.readArray <<< Raw.unsafeToForeign

instance Deserialize a => Deserialize (Maybe a) where
  deserialize raw =
    let
      nothing = const Nothing <$> deserialize @Null raw
      just = Just <$> deserialize raw
    in
      just <|> nothing
