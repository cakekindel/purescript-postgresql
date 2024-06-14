module Test.Data.Postgres where

import Prelude

import Control.Monad.Gen (chooseFloat, chooseInt, elements, oneOf)
import Control.Parallel (parTraverse_)
import Data.Array (intercalate)
import Data.Array as Array
import Data.Array.NonEmpty as Array.NonEmpty
import Data.DateTime (DateTime(..), canonicalDate)
import Data.Enum (toEnum)
import Data.Foldable (fold)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Number (abs) as Number
import Data.Postgres (class Rep)
import Data.Postgres.Interval (Interval)
import Data.Postgres.Interval as Interval
import Data.Postgres.Query.Builder as Q
import Data.Postgres.Raw (Raw, jsNull)
import Data.Postgres.Raw as Raw
import Data.Postgres.Result (class FromRow)
import Data.String as String
import Data.Time (Time(..))
import Data.Time.Duration (class Duration, Days, Hours, Milliseconds, Minutes, Seconds)
import Data.Traversable (for, sequence)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Postgres.Client (exec, query)
import Effect.Class (liftEffect)
import Effect.Postgres.Client (Client)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Object as Object
import JS.BigInt (BigInt)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Partial.Unsafe (unsafePartial)
import Simple.JSON (writeJSON)
import Test.Common (withPoolClient)
import Test.QuickCheck (class Arbitrary, arbitrary, randomSeed)
import Test.QuickCheck.Gen (sample, vectorOf)
import Test.Spec (Spec, SpecT, around, describe, it)
import Test.Spec.Assertions (fail)

foreign import readBigInt64BE :: Buffer -> Effect BigInt
foreign import dbg :: forall a. a -> Effect Unit

newtype GenIntervalSubMonth = GenIntervalSubMonth Interval

derive instance Newtype GenIntervalSubMonth _
instance Arbitrary GenIntervalSubMonth where
  arbitrary = do
    days <- chooseInt 0 30
    hours <- chooseInt 0 23
    minutes <- chooseInt 0 59
    seconds <- chooseInt 0 59
    milliseconds <- chooseFloat 0.0 999.9
    pure $ wrap $ Interval.make $ Interval.zero {days = days, hours = hours, minutes = minutes, seconds = seconds, milliseconds = milliseconds}

newtype GenInterval = GenInterval Interval

derive instance Newtype GenInterval _
instance Arbitrary GenInterval where
  arbitrary = do
    years <- chooseInt 0 10
    months <- chooseInt 0 11
    days <- chooseInt 0 30
    hours <- chooseInt 0 23
    minutes <- chooseInt 0 59
    seconds <- chooseInt 0 59
    milliseconds <- chooseFloat 0.0 999.9
    pure $ wrap $ Interval.make {years, months, days, hours, minutes, seconds, milliseconds}

newtype GenSmallInt = GenSmallInt Int

derive instance Newtype GenSmallInt _
instance Arbitrary GenSmallInt where
  arbitrary = wrap <$> chooseInt (-32768) 32767

newtype GenDateTime = GenDateTime DateTime

derive instance Newtype GenDateTime _
instance Arbitrary GenDateTime where
  arbitrary = do
    yr <- chooseInt 1970 2100
    mo <- chooseInt 1 12
    dy <- chooseInt 1 28
    hr <- chooseInt 0 23
    mn <- chooseInt 0 59
    sc <- chooseInt 0 59
    ms <- chooseInt 0 999
    let
      date = unsafePartial fromJust $ Just canonicalDate <*> toEnum yr <*> toEnum mo <*> toEnum dy
      time = unsafePartial fromJust $ Just Time <*> toEnum hr <*> toEnum mn <*> toEnum sc <*> toEnum ms
    pure $ wrap $ DateTime date time

newtype GenString = GenString String

derive instance Newtype GenString _
instance Arbitrary GenString where
  arbitrary = do
    let chars = unsafePartial fromJust $ Array.NonEmpty.fromArray $ String.split (wrap "") "abcdefghijklmnopqrstuvwxyz01234567890 _-=><|\\/"
    len <- chooseInt 0 100
    chars' <- vectorOf len (elements chars)
    pure $ wrap $ fold chars'

newtype GenSmallFloat = GenSmallFloat Number

derive instance Newtype GenSmallFloat _
instance Arbitrary GenSmallFloat where
  arbitrary = do
    let byte = chooseInt 0 7
    bytes <- sequence $ Array.replicate 4 byte
    pure
      $ wrap
      $ unsafePerformEffect do
          buf <- Buffer.fromArray bytes
          Buffer.read Buffer.FloatBE 0 buf

newtype GenBigInt = GenBigInt BigInt

derive instance Newtype GenBigInt _

instance Arbitrary GenBigInt where
  arbitrary = do
    let byte = chooseInt 0 7
    bytes <- sequence $ Array.replicate 8 byte
    let
      bigint = unsafePerformEffect do
        buf <- Buffer.fromArray bytes
        readBigInt64BE buf
    pure $ wrap bigint

newtype GenJSON = GenJSON Foreign

derive instance Newtype GenJSON _

instance Arbitrary GenJSON where
  arbitrary =
    let
      json _ = map wrap $ oneOf' [ prim, array unit, obj unit ]
      oneOf' = oneOf <<< unsafePartial fromJust <<< Array.NonEmpty.fromArray
      elements' = elements <<< unsafePartial fromJust <<< Array.NonEmpty.fromArray
      prim = oneOf'
        [ pure $ unsafeToForeign jsNull
        , unsafeToForeign <$> arbitrary @Number
        , unsafeToForeign <$> arbitrary @String
        ]
      array _ = map unsafeToForeign $ vectorOf 3 prim
      obj _ = do
        keys <- vectorOf 3 (elements' [ "foo", "bar", "baz", "quux", "duck", "dog", "cat", "cow" ])
        kvs <- for keys \k -> (k /\ _) <$> prim
        pure $ unsafeToForeign $ Object.fromFoldable kvs
    in
      json unit

asRaw :: forall a. a -> Raw
asRaw = Raw.unsafeFromForeign <<< unsafeToForeign

type PursType = String
type SQLType = String
type FromArbitrary x a = x -> a
type IsEqual a = a -> a -> Boolean

class (Show a, FromRow a, Rep a) <= Checkable a

instance (Show a, FromRow a, Rep a) => Checkable a

spec :: Spec Unit
spec =
  let
    check
      :: forall @a @x
       . Checkable a
      => Arbitrary x
      => { purs :: String
         , sql :: String
         , fromArb :: x -> a
         , isEq :: a -> a -> Boolean
         }
      -> SpecT Aff Client Identity Unit
    check { purs, sql, fromArb, isEq } =
      it (purs <> " <> " <> sql) \c -> do
        let
          tab =
            String.replace (wrap " ") (wrap "_")
              $ String.replace (wrap "[") (wrap "")
              $ String.replace (wrap "]") (wrap "")
              $ sql <> "_is_" <> String.toLower purs
          createtab =
            intercalate "\n"
              [ "create temp table " <> tab
              , "  ( val " <> sql
              , "  );"
              ]
          ser x =
            Q.build do
              x' <- Q.param $ fromArb x
              let val = x' <> " :: " <> sql
              pure $ "insert into " <> tab <> " values (" <> val <> ")"
          de x =
            Q.build do
              x' <- Q.param $ fromArb x
              let val = x' <> " :: " <> sql
              pure $ "select " <> val
        void $ exec createtab c
        seed <- liftEffect randomSeed
        let xs = sample seed 10 (arbitrary @x)
        flip parTraverse_ xs
          \x -> do
            void $ exec (ser x) c
            res <- query (de x) c
            let
              exp = fromArb x
              act = unsafePartial fromJust $ Array.head res
            when (not $ isEq exp act) $ fail $ "expected " <> show exp <> " to equal " <> show act
  in
    around withPoolClient
      $ describe "Data.Postgres"
      $ do
          let
            durationFromGenInterval :: forall d. Semigroup d => Duration d => Newtype d Number => GenIntervalSubMonth -> d
            durationFromGenInterval = fromMaybe (wrap 0.0) <<< Interval.toDuration <<< unwrap
            durationEq :: forall d. Duration d => Newtype d Number => d -> d -> Boolean
            durationEq a b = Number.abs (unwrap a - unwrap b) <= 0.001
          check @Milliseconds @GenIntervalSubMonth { purs: "Milliseconds", sql: "interval", fromArb: durationFromGenInterval, isEq: durationEq}
          check @Seconds @GenIntervalSubMonth { purs: "Seconds", sql: "interval", fromArb: durationFromGenInterval, isEq: durationEq}
          check @Minutes @GenIntervalSubMonth { purs: "Minutes", sql: "interval", fromArb: durationFromGenInterval, isEq: durationEq}
          check @Hours @GenIntervalSubMonth { purs: "Hours", sql: "interval", fromArb: durationFromGenInterval, isEq: durationEq}
          check @Days @GenIntervalSubMonth { purs: "Days", sql: "interval", fromArb: durationFromGenInterval, isEq: durationEq}

          check @Int @GenSmallInt { purs: "Int", sql: "int2", fromArb: unwrap, isEq: eq }
          check @Int { purs: "Int", sql: "int4", fromArb: identity, isEq: eq }
          check @String @GenString { purs: "String", sql: "text", fromArb: unwrap, isEq: eq }
          check @Boolean { purs: "Boolean", sql: "bool", fromArb: identity, isEq: eq }
          check @Number @GenSmallFloat { purs: "Number", sql: "float4", fromArb: unwrap, isEq: \a b -> Number.abs (a - b) <= 0.0001 }
          check @Number { purs: "Number", sql: "float8", fromArb: identity, isEq: eq }
          check @BigInt @GenBigInt { purs: "BigInt", sql: "int8", fromArb: unwrap, isEq: eq }
          check @DateTime @GenDateTime { purs: "DateTime", sql: "timestamptz", fromArb: unwrap, isEq: eq }

          check @(Maybe String) @(Maybe GenString) { purs: "Maybe String", sql: "text", fromArb: map unwrap, isEq: eq }
          check @(Array String) @(Array GenString) { purs: "Array String", sql: "text[]", fromArb: map unwrap, isEq: eq }

          check @String @GenJSON { purs: "JSON", sql: "json", fromArb: writeJSON <<< unwrap, isEq: eq }
