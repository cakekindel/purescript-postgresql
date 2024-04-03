module Data.Postgres.Range where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Foldable (class Foldable, foldl)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Postgres.Raw (Raw, rawMaybeNull, rawNullMaybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, foldMapDefaultL, foldrDefault, sequenceDefault, traverse)
import Effect (Effect)

-- | A range of values with optional upper & lower bounds.
-- |
-- | * `mempty -> '(,)'`
-- | * `gte 1 -> '[1,)'`
-- | * `lt 2 -> '(,2]'`
-- | * `gte 1 <> lt 2 -> '[1,2)'`
newtype Range a = Range { upper :: Maybe (Bound a), lower :: Maybe (Bound a) }

derive instance Functor Range
instance Foldable Range where
  foldl f b r = foldl f b $ boundValue <$> Array.catMaybes [ upper r, lower r ]
  foldr f b r = foldrDefault f b r
  foldMap f r = foldMapDefaultL f r

instance Traversable Range where
  traverse f r =
    let
      build u l = Range { upper: u, lower: l }
      fToBound = traverse (traverse f)
    in
      pure build <*> fToBound (upper r) <*> fToBound (lower r)
  sequence = sequenceDefault

derive instance Generic (Range a) _
derive instance Newtype (Range a) _
derive instance Eq a => Eq (Range a)
instance Show a => Show (Range a) where
  show = genericShow

instance Monoid (Range a) where
  mempty = Range { upper: Nothing, lower: Nothing }

instance Semigroup (Range a) where
  append (Range { upper: au, lower: al }) (Range { upper: bu, lower: bl }) = Range ({ upper: bu <|> au, lower: bl <|> al })

-- | An upper or lower range bound
data Bound a = BoundIncl a | BoundExcl a

instance Foldable Bound where
  foldl f b (BoundIncl a) = f b a
  foldl f b (BoundExcl a) = f b a
  foldr f b a = foldrDefault f b a
  foldMap f r = foldMapDefaultL f r

instance Traversable Bound where
  traverse f (BoundIncl a) = BoundIncl <$> f a
  traverse f (BoundExcl a) = BoundExcl <$> f a
  sequence = sequenceDefault

derive instance Functor Bound
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
__rangeFromRecord :: RangeRecord -> Range Raw
__rangeFromRecord raw = Range { upper: makeBound raw.upperIncl <$> rawNullMaybe raw.upper, lower: makeBound raw.lowerIncl <$> rawNullMaybe raw.lower }

-- | FFI
__rangeToRecord :: Range Raw -> RangeRecord
__rangeToRecord r = { upper: rawMaybeNull $ boundValue <$> upper r, lower: rawMaybeNull $ boundValue <$> lower r, upperIncl: fromMaybe false $ boundIsInclusive <$> upper r, lowerIncl: fromMaybe false $ boundIsInclusive <$> lower r }
