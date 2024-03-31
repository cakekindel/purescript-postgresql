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

-- | A range of values with optional upper & lower bounds.
-- |
-- | * `mempty -> '(,)'`
-- | * `gte 1 -> '[1,)'`
-- | * `lt 2 -> '(,2]'`
-- | * `gte 1 <> lt 2 -> '[1,2)'`
newtype Range a = Range { upper :: Maybe (Bound a), lower :: Maybe (Bound a) }

derive instance Generic (Range a) _
derive instance Newtype (Range a) _
derive instance Eq a => Eq (Range a)
instance Show a => Show (Range a) where
  show = genericShow

instance (Ord a, Rep a) => Serialize (Range a) where
  serialize = map (Raw.unsafeFromForeign <<< unsafeToForeign <<< __rangeRawFromRecord) <<< __rangeToRecord

instance (Ord a, Rep a) => Deserialize (Range a) where
  deserialize = __rangeFromRecord <=< map __rangeRawToRecord <<< lift <<< __rangeRawFromRaw

instance Monoid (Range a) where
  mempty = Range { upper: Nothing, lower: Nothing }

instance Semigroup (Range a) where
  append (Range { upper: au, lower: al }) (Range { upper: bu, lower: bl }) = Range ({ upper: bu <|> au, lower: bl <|> al })

-- | An upper or lower range bound
data Bound a = BoundIncl a | BoundExcl a

derive instance Generic (Bound a) _
derive instance Eq a => Eq (Bound a)
instance Show a => Show (Bound a) where
  show = genericShow

-- | Get the value of the bound
boundValue :: forall a. Bound a -> a
boundValue (BoundIncl a) = a
boundValue (BoundExcl a) = a

-- | Whether this bound includes the value `a`
boundIsInclusive :: forall a. Bound a -> Boolean
boundIsInclusive (BoundIncl _) = true
boundIsInclusive (BoundExcl _) = false

-- | The upper bound of a range
upper :: forall a. Range a -> Maybe (Bound a)
upper = _.upper <<< unwrap

-- | The lower bound of a range
lower :: forall a. Range a -> Maybe (Bound a)
lower = _.lower <<< unwrap

-- | Creates a bound from a bool indicating if the bound is inclusive
-- | and a value `a`
makeBound :: forall a. Boolean -> a -> Bound a
makeBound i a
  | i = BoundIncl a
  | otherwise = BoundExcl a

-- | Attempt to parse a SQL string of a range as `Range a`
parseSQL :: forall a. Rep a => (String -> RepT a) -> String -> RepT (Range a)
parseSQL fromString sql = do
  range <- lift $ __rangeRawParse sql $ smash <<< (serialize <=< fromString)
  __rangeFromRecord $ __rangeRawToRecord range

-- | Serialize a `Range` as a SQL string
printSQL :: forall a. Rep a => Range a -> RepT String
printSQL range = do
  record <- __rangeToRecord range
  lift $ __rangeRawSerialize $ __rangeRawFromRecord record

-- | Returns whether the range contains value `a`
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

-- | Creates a range with no upper bound and inclusive lower bound `a`; `[a,)`
gte :: forall a. Ord a => a -> Range a
gte a = Range { lower: Just $ BoundIncl a, upper: Nothing }

-- | Creates a range with no upper bound and exclusive lower bound `a`; `(a,)`
gt :: forall a. Ord a => a -> Range a
gt a = Range { lower: Just $ BoundExcl a, upper: Nothing }

-- | Creates a range with no lower bound and inclusive upper bound `a`; `(,a]`
lt :: forall a. Ord a => a -> Range a
lt a = Range { lower: Nothing, upper: Just $ BoundExcl a }

-- | Creates a range with no lower bound and exclusive upper bound `a`; `(,a)`
lte :: forall a. Ord a => a -> Range a
lte a = Range { lower: Nothing, upper: Just $ BoundIncl a }

-- | FFI
type RangeRecord = { upper :: Raw, lower :: Raw, lowerIncl :: Boolean, upperIncl :: Boolean }

-- | FFI
foreign import data RangeRaw :: Type
-- | FFI
foreign import __rangeRawFromRaw :: Raw -> Effect RangeRaw
-- | FFI
foreign import __rangeRawToRecord :: RangeRaw -> RangeRecord
-- | FFI
foreign import __rangeRawFromRecord :: RangeRecord -> RangeRaw
-- | FFI
foreign import __rangeRawParse :: String -> (String -> Effect Raw) -> Effect RangeRaw
-- | FFI
foreign import __rangeRawSerialize :: RangeRaw -> Effect String

-- | FFI
__rangeFromRecord :: forall a. Deserialize a => RangeRecord -> RepT (Range a)
__rangeFromRecord raw = do
  upper' :: Maybe a <- deserialize raw.upper
  lower' :: Maybe a <- deserialize raw.lower
  pure $ Range { upper: makeBound raw.upperIncl <$> upper', lower: makeBound raw.lowerIncl <$> lower' }

-- | FFI
__rangeToRecord :: forall a. Serialize a => Range a -> RepT RangeRecord
__rangeToRecord r = do
  upper' <- serialize $ boundValue <$> upper r
  lower' <- serialize $ boundValue <$> lower r
  pure $ { upper: upper', lower: lower', upperIncl: fromMaybe false $ boundIsInclusive <$> upper r, lowerIncl: fromMaybe false $ boundIsInclusive <$> lower r }

