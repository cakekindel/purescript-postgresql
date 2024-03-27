module Data.Postgres.Range where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Trans.Class (lift)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Postgres (class Deserialize, class Rep, class Serialize, RepT, deserialize, serialize, smash)
import Data.Postgres.Raw (Raw)
import Data.Postgres.Raw as Raw
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Foreign (unsafeToForeign)

type RangeRawRecord = { upper :: Raw, lower :: Raw, lowerIncl :: Boolean, upperIncl :: Boolean }

foreign import data RangeRaw :: Type
foreign import readRangeRaw :: Raw -> Effect RangeRaw
foreign import rangeRawToRecord :: RangeRaw -> RangeRawRecord
foreign import rangeRawFromRecord :: RangeRawRecord -> RangeRaw
foreign import rangeRawParse :: String -> (String -> Effect Raw) -> Effect RangeRaw
foreign import rangeRawSerialize :: RangeRaw -> Effect String

rangeFromRaw :: forall a. Deserialize a => RangeRawRecord -> RepT (Range a)
rangeFromRaw raw = do
  upper' :: Maybe a <- deserialize raw.upper
  lower' :: Maybe a <- deserialize raw.lower
  pure $ Range { upper: makeBound raw.upperIncl <$> upper', lower: makeBound raw.lowerIncl <$> lower' }

rangeToRaw :: forall a. Serialize a => Range a -> RepT RangeRawRecord
rangeToRaw r = do
  upper' <- serialize $ boundValue <$> upper r
  lower' <- serialize $ boundValue <$> lower r
  pure $ { upper: upper', lower: lower', upperIncl: fromMaybe false $ boundIsInclusive <$> upper r, lowerIncl: fromMaybe false $ boundIsInclusive <$> lower r }

data Bound a = BoundIncl a | BoundExcl a

derive instance Generic (Bound a) _
derive instance Eq a => Eq (Bound a)
instance Show a => Show (Bound a) where
  show = genericShow

boundValue :: forall a. Bound a -> a
boundValue (BoundIncl a) = a
boundValue (BoundExcl a) = a

boundIsInclusive :: forall a. Bound a -> Boolean
boundIsInclusive (BoundIncl _) = true
boundIsInclusive (BoundExcl _) = false

upper :: forall a. Range a -> Maybe (Bound a)
upper = _.upper <<< unwrap

lower :: forall a. Range a -> Maybe (Bound a)
lower = _.lower <<< unwrap

makeBound :: forall a. Boolean -> a -> Bound a
makeBound i a
  | i = BoundIncl a
  | otherwise = BoundExcl a

newtype Range a = Range { upper :: Maybe (Bound a), lower :: Maybe (Bound a) }

derive instance Generic (Range a) _
derive instance Newtype (Range a) _
derive instance Eq a => Eq (Range a)
instance Show a => Show (Range a) where
  show = genericShow

instance (Ord a, Rep a) => Serialize (Range a) where
  serialize a = do
    raw <- rangeToRaw a
    pure $ Raw.unsafeFromForeign $ unsafeToForeign $ rangeRawFromRecord raw

instance (Ord a, Rep a) => Deserialize (Range a) where
  deserialize raw = do
    range :: RangeRaw <- lift $ readRangeRaw raw
    rangeFromRaw $ rangeRawToRecord range

instance Monoid (Range a) where
  mempty = Range { upper: Nothing, lower: Nothing }

instance Semigroup (Range a) where
  append (Range { upper: au, lower: al }) (Range { upper: bu, lower: bl }) = Range ({ upper: bu <|> au, lower: bl <|> al })

parseSQL :: forall a. Rep a => (String -> RepT a) -> String -> RepT (Range a)
parseSQL fromString sql = do
  range <- lift $ rangeRawParse sql $ smash <<< (serialize <=< fromString)
  rangeFromRaw $ rangeRawToRecord range

printSQL :: forall a. Rep a => Range a -> RepT String
printSQL range = do
  record <- rangeToRaw range
  lift $ rangeRawSerialize $ rangeRawFromRecord record

contains :: forall a. Ord a => a -> Range a -> Boolean
contains a r =
  let
    upperOk
      | Just (BoundIncl u) <- upper r = u >= a
      | Just (BoundExcl u) <- upper r = u > a
      | otherwise = true
    lowerOk
      | Just (BoundIncl u) <- lower r = u <= a
      | Just (BoundExcl u) <- lower r = u < a
      | otherwise = true
  in
    upperOk && lowerOk

gte :: forall a. Ord a => a -> Range a
gte a = Range { upper: Just $ BoundIncl a, lower: Nothing }

gt :: forall a. Ord a => a -> Range a
gt a = Range { upper: Just $ BoundExcl a, lower: Nothing }

lt :: forall a. Ord a => a -> Range a
lt a = Range { upper: Nothing, lower: Just $ BoundExcl a }

lte :: forall a. Ord a => a -> Range a
lte a = Range { upper: Nothing, lower: Just $ BoundIncl a }
