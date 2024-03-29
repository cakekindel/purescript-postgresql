module Test.Data.Postgres where

import Prelude

import Control.Monad.Gen (chooseInt, elements, oneOf)
import Data.Array as Array
import Data.Array.NonEmpty as Array.NonEmpty
import Data.DateTime (DateTime(..), canonicalDate)
import Data.DateTime.Instant as Instant
import Data.Enum (toEnum)
import Data.Foldable (fold)
import Data.Identity (Identity)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Number (abs) as Number
import Data.Postgres (class Rep, jsNull)
import Data.Postgres.Query.Builder as Q
import Data.Postgres.Raw (Raw)
import Data.Postgres.Raw as Raw
import Data.Postgres.Result (class FromResult)
import Data.RFC3339String as DateTime.ISO
import Data.String as String
import Data.Time (Time(..))
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
import JS.BigInt as BigInt
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Partial.Unsafe (unsafePartial)
import Simple.JSON (writeJSON)
import Test.Common (withClient)
import Test.QuickCheck (class Arbitrary, arbitrary, randomSeed)
import Test.QuickCheck.Gen (sample, vectorOf)
import Test.Spec (Spec, SpecT, around, describe, it)
import Test.Spec.Assertions (fail)

foreign import readBigInt64BE :: Buffer -> Effect BigInt
foreign import dbg :: forall a. a -> Effect Unit

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

spec :: Spec Unit
spec =
  let
    check :: forall @a @x. Show a => Arbitrary x => Rep a => FromResult a => String -> String -> (x -> a) -> (a -> Raw) -> (a -> a -> Boolean) -> SpecT Aff Client Identity Unit
    check purs sql xa asRaw_ isEq =
      it (purs <> " <> " <> sql) \c -> do
        let
          tab = String.replace (wrap " ") (wrap "_") $ String.replace (wrap "[") (wrap "") $ String.replace (wrap "]") (wrap "") $ sql <> "_is_" <> String.toLower purs
          ser x =
            Q.build do
              x' <- Q.param $ xa x
              pure $ "insert into " <> tab <> " values (" <> x' <> " :: " <> sql <> ")"
          de x =
            Q.build do
              x' <- Q.param $ xa x
              pure $ "select " <> x' <> " :: " <> sql
        void $ exec ("create temp table " <> tab <> " (val " <> sql <> ")") c
        seed <- liftEffect randomSeed
        let
          xs = sample seed 20 (arbitrary @x)
        void $ for xs \x -> do
          void $ exec (ser x) c
          res :: Array a <- query (de x) c
          let
            exp = xa x
            act = unsafePartial fromJust $ Array.head res
          when (not $ isEq exp act) $ fail $ "expected " <> show exp <> " to equal " <> show act

    check_ :: forall @a. Eq a => Show a => Arbitrary a => FromResult a => Rep a => String -> String -> SpecT Aff Client Identity Unit
    check_ purs sql = check @a @a purs sql identity asRaw eq

    dateTimeFromArbitrary :: Int -> DateTime
    dateTimeFromArbitrary = Instant.toDateTime <<< unsafePartial fromJust <<< Instant.instant <<< wrap <<< Int.toNumber
  in
    around withClient
      $ describe "Data.Postgres"
      $ do
          check @Int @GenSmallInt "Int" "int2" unwrap asRaw eq
          check_ @Int "Int" "int4"

          check @String @GenString "String" "text" unwrap asRaw eq

          check_ @Boolean "Boolean" "bool"

          check @Number @GenSmallFloat "Number" "float4" unwrap asRaw (\a b -> Number.abs (a - b) <= 0.0001)
          check_ @Number "Number" "float8"

          check @BigInt @GenBigInt "BigInt" "int8" unwrap (asRaw <<< BigInt.toString) eq
          check @(Maybe String) @(Maybe GenString) "Maybe String" "text" (map unwrap) (maybe jsNull asRaw) eq
          check @(Array String) @(Array GenString) "Array String" "text[]" (map unwrap) asRaw eq
          check @DateTime @GenDateTime "DateTime" "timestamptz" unwrap (asRaw <<< DateTime.ISO.fromDateTime) eq
          check @String @GenJSON "JSON" "json" (writeJSON <<< unwrap) asRaw eq
