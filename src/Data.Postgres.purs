module Data.Postgres where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftEither, liftMaybe)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Morph (hoist)
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Postgres.Raw (Raw)
import Data.Postgres.Raw (unsafeFromForeign, unsafeToForeign) as Raw
import Data.RFC3339String as DateTime.ISO
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Exception (error)
import Foreign (ForeignError(..))
import Foreign as F
import Node.Buffer (Buffer)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON', writeJSON)

newtype JSON a = JSON a

derive instance Newtype (JSON a) _
derive newtype instance WriteForeign a => WriteForeign (JSON a)
derive newtype instance ReadForeign a => ReadForeign (JSON a)

foreign import null_ :: Raw

-- | This mutates `import('pg').types`, setting deserialization
-- | for some types to unmarshal as strings rather than JS values.
foreign import modifyPgTypes :: Effect Unit

-- | The SQL value NULL
data Null = Null

derive instance Generic Null _
derive instance Eq Null
derive instance Ord Null
instance Show Null where
  show = genericShow

-- | The serialization & deserialization monad.
type RepT a = ExceptT (NonEmptyList ForeignError) Effect a

-- | Flatten to an Effect, rendering any `RepError`s to `String` using `Show`.
smash :: forall a. RepT a -> Effect a
smash = liftEither <=< map (lmap (error <<< show)) <<< runExceptT

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

-- | Serializes as `Null`.
instance Serialize Unit where
  serialize _ = serialize Null

instance Serialize Null where
  serialize _ = unsafeSerializeCoerce null_

instance WriteForeign a => Serialize (JSON a) where
  serialize = serialize <<< writeJSON <<< unwrap

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

-- | Note: this will always succeed, discarding
-- | the actual raw value yielded.
-- |
-- | To explicitly deserialize NULL values and fail
-- | when the value is non-null, use `Null`.
instance Deserialize Unit where
  deserialize _ = pure unit

instance Deserialize Null where
  deserialize = map (const Null) <<< F.readNullOrUndefined <<< Raw.unsafeToForeign

instance ReadForeign a => Deserialize (JSON a) where
  deserialize = map wrap <<< (hoist (pure <<< unwrap) <<< readJSON') <=< deserialize @String

-- | `bytea`
instance Deserialize Buffer where
  deserialize = (F.unsafeReadTagged "Buffer") <<< Raw.unsafeToForeign

instance Deserialize Int where
  deserialize = F.readInt <<< Raw.unsafeToForeign

instance Deserialize Boolean where
  deserialize = F.readBoolean <<< Raw.unsafeToForeign

instance Deserialize String where
  deserialize = F.readString <<< Raw.unsafeToForeign

instance Deserialize Number where
  deserialize = F.readNumber <<< Raw.unsafeToForeign

instance Deserialize Char where
  deserialize = F.readChar <<< Raw.unsafeToForeign

instance Deserialize DateTime where
  deserialize raw = do
    s :: String <- deserialize raw
    let invalid = pure $ ForeignError $ "Not a valid ISO8601 string: `" <> s <> "`"
    liftMaybe invalid $ DateTime.ISO.toDateTime $ wrap s

instance Deserialize a => Deserialize (Array a) where
  deserialize = traverse (deserialize <<< Raw.unsafeFromForeign) <=< F.readArray <<< Raw.unsafeToForeign

instance Deserialize a => Deserialize (Maybe a) where
  deserialize raw =
    let
      nothing = const Nothing <$> deserialize @Null raw
      just = Just <$> deserialize raw
    in
      just <|> nothing
