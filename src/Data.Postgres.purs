module Data.Postgres where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftEither, liftMaybe, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Postgres.Interval (Interval)
import Data.Postgres.Interval as Interval
import Data.Postgres.Range (Range, __rangeFromRecord, __rangeRawFromRaw, __rangeRawFromRecord, __rangeRawToRecord, __rangeToRecord)
import Data.Postgres.Raw (Null(..), Raw, jsNull)
import Data.Postgres.Raw (unsafeFromForeign, asForeign) as Raw
import Data.RFC3339String as DateTime.ISO
import Data.Time.Duration (Days, Hours, Milliseconds, Minutes, Seconds)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Exception (error)
import Foreign (ForeignError(..), tagOf, unsafeFromForeign, unsafeToForeign)
import Foreign as F
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Node.Buffer (Buffer)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON', writeJSON)

-- | Newtype hinting that this value should be serialized / deserialized as a JSON string.
newtype JSON a = JSON a

derive instance Newtype (JSON a) _
derive newtype instance Show a => Show (JSON a)
derive newtype instance Eq a => Eq (JSON a)
derive newtype instance Ord a => Ord (JSON a)
derive newtype instance WriteForeign a => WriteForeign (JSON a)
derive newtype instance ReadForeign a => ReadForeign (JSON a)

-- | This mutates `import('pg').types`, setting deserialization
-- | for some types to unmarshal as strings rather than JS values.
foreign import modifyPgTypes :: Effect Unit

foreign import isInstanceOfBuffer :: F.Foreign -> Boolean
foreign import isInstanceOfInterval :: F.Foreign -> Boolean

-- | The serialization & deserialization monad.
type RepT = ExceptT (NonEmptyList ForeignError) Effect

-- | Flatten to an Effect, `show`ing errors
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

-- | Coerces the value to `Raw`.
-- |
-- | This is only safe for values whose javascript representation
-- | can be directly serialized by `node-postgres` to the corresponding
-- | SQL type.
unsafeSerializeCoerce :: forall m a. Monad m => a -> m Raw
unsafeSerializeCoerce = pure <<< Raw.unsafeFromForeign <<< F.unsafeToForeign

invalidDuration :: NonEmptyList ForeignError
invalidDuration = pure $ ForeignError $ "Can't convert interval with year or month components to Milliseconds"

instance Serialize Raw where
  serialize = pure

-- | `NULL`
instance Serialize Unit where
  serialize _ = serialize Null

-- | `NULL`
instance Serialize Null where
  serialize _ = unsafeSerializeCoerce jsNull

-- | `json`, `jsonb`
instance WriteForeign a => Serialize (JSON a) where
  serialize = serialize <<< writeJSON <<< unwrap

-- | `bytea`
instance Serialize Buffer where
  serialize = unsafeSerializeCoerce

-- | `int2`, `int4`
instance Serialize Int where
  serialize = unsafeSerializeCoerce

-- | `int8`
instance Serialize BigInt where
  serialize = serialize <<< BigInt.toString

-- | `bool`
instance Serialize Boolean where
  serialize = unsafeSerializeCoerce

-- | `text`, `inet`, `tsquery`, `tsvector`, `uuid`, `xml`, `cidr`, `time`, `timetz`
instance Serialize String where
  serialize = unsafeSerializeCoerce

-- | `float4`, `float8`
instance Serialize Number where
  serialize = unsafeSerializeCoerce

-- | `interval`
instance Serialize DateTime where
  serialize = serialize <<< unwrap <<< DateTime.ISO.fromDateTime

-- | `interval`
instance Serialize Interval where
  serialize = unsafeSerializeCoerce

-- | `interval`
instance Serialize Milliseconds where
  serialize = serialize <<< Interval.fromDuration

-- | `interval`
instance Serialize Seconds where
  serialize = serialize <<< Interval.fromDuration

-- | `interval`
instance Serialize Minutes where
  serialize = serialize <<< Interval.fromDuration

-- | `interval`
instance Serialize Hours where
  serialize = serialize <<< Interval.fromDuration

-- | `interval`
instance Serialize Days where
  serialize = serialize <<< Interval.fromDuration

-- | `timestamp`, `timestamptz`
instance Serialize Instant where
  serialize = serialize <<< Instant.toDateTime

-- | `Just` -> `a`, `Nothing` -> `NULL`
instance Serialize a => Serialize (Maybe a) where
  serialize (Just a) = serialize a
  serialize Nothing = unsafeSerializeCoerce jsNull

-- | postgres `array`
instance Serialize a => Serialize (Array a) where
  serialize = unsafeSerializeCoerce <=< traverse serialize

instance (Ord a, Rep a) => Serialize (Range a) where
  serialize =
    map (Raw.unsafeFromForeign <<< unsafeToForeign <<< __rangeRawFromRecord <<< __rangeToRecord)
      <<< traverse serialize

instance Deserialize Raw where
  deserialize = pure

-- | `NULL` (always succeeds)
instance Deserialize Unit where
  deserialize _ = pure unit

-- | `NULL` (fails if non-null)
instance Deserialize Null where
  deserialize = map (const Null) <<< F.readNullOrUndefined <<< Raw.asForeign

-- | `json`, `jsonb`
instance ReadForeign a => Deserialize (JSON a) where
  deserialize = map wrap <<< (hoist (pure <<< unwrap) <<< readJSON') <=< deserialize @String

-- | `bytea`
instance Deserialize Buffer where
  deserialize =
    let
      notBuffer a = pure $ TypeMismatch (tagOf a) "Buffer"
      readBuffer a = when (not $ isInstanceOfBuffer a) (throwError $ notBuffer a) $> unsafeFromForeign a
    in
      readBuffer <<< Raw.asForeign

-- | `interval`
instance Deserialize Interval where
  deserialize =
    let
      notInterval a = pure $ TypeMismatch (tagOf a) "Interval"
      readInterval a = when (not $ isInstanceOfInterval a) (throwError $ notInterval a) $> unsafeFromForeign a
    in
      readInterval <<< Raw.asForeign

-- | `interval`
instance Deserialize Milliseconds where
  deserialize = flip bind (liftMaybe invalidDuration) <<< map Interval.toDuration <<< deserialize

-- | `interval`
instance Deserialize Seconds where
  deserialize = flip bind (liftMaybe invalidDuration) <<< map Interval.toDuration <<< deserialize

-- | `interval`
instance Deserialize Minutes where
  deserialize = flip bind (liftMaybe invalidDuration) <<< map Interval.toDuration <<< deserialize

-- | `interval`
instance Deserialize Hours where
  deserialize = flip bind (liftMaybe invalidDuration) <<< map Interval.toDuration <<< deserialize

-- | `interval`
instance Deserialize Days where
  deserialize = flip bind (liftMaybe invalidDuration) <<< map Interval.toDuration <<< deserialize

-- | `int2`, `int4`
instance Deserialize Int where
  deserialize = F.readInt <<< Raw.asForeign

-- | `int8`
instance Deserialize BigInt where
  deserialize =
    let
      invalid s = pure $ ForeignError $ "Invalid bigint: " <> s
      fromString s = liftMaybe (invalid s) $ BigInt.fromString s
    in
      fromString <=< deserialize @String

-- | `bool`
instance Deserialize Boolean where
  deserialize = F.readBoolean <<< Raw.asForeign

-- | `text`, `inet`, `tsquery`, `tsvector`, `uuid`, `xml`, `cidr`, `time`, `timetz`
instance Deserialize String where
  deserialize = F.readString <<< Raw.asForeign

-- | `float4`, `float8`
instance Deserialize Number where
  deserialize = F.readNumber <<< Raw.asForeign

-- | `timestamp`, `timestamptz`
instance Deserialize DateTime where
  deserialize raw = do
    s :: String <- deserialize raw
    let invalid = pure $ ForeignError $ "Not a valid ISO8601 string: `" <> s <> "`"
    liftMaybe invalid $ DateTime.ISO.toDateTime $ wrap s

-- | `timestamp`, `timestamptz`
instance Deserialize Instant where
  deserialize = map Instant.fromDateTime <<< deserialize

-- | postgres `array`
instance Deserialize a => Deserialize (Array a) where
  deserialize = traverse (deserialize <<< Raw.unsafeFromForeign) <=< F.readArray <<< Raw.asForeign

-- | non-NULL -> `Just`, NULL -> `Nothing`
instance Deserialize a => Deserialize (Maybe a) where
  deserialize raw =
    let
      nothing = const Nothing <$> deserialize @Null raw
      just = Just <$> deserialize raw
    in
      just <|> nothing

instance (Ord a, Rep a) => Deserialize (Range a) where
  deserialize = traverse deserialize <=< map (__rangeFromRecord <<< __rangeRawToRecord) <<< lift <<< __rangeRawFromRaw
