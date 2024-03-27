module Test.Data.Postgres where

import Prelude

import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (runExceptT)
import Data.DateTime (DateTime(..))
import Data.DateTime.Instant as Instant
import Data.Int as Int
import Data.Maybe (Maybe, fromJust, fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Postgres (class Rep, JSON(..), Null(..), deserialize, null_, serialize, smash)
import Data.Postgres.Range as Range
import Data.Postgres.Raw (Raw)
import Data.Postgres.Raw as Raw
import Data.RFC3339String as DateTime.ISO
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (unsafeToForeign)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartial)
import Simple.JSON (writeImpl, writeJSON)
import Test.QuickCheck (class Arbitrary, (==?))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)

asRaw :: forall a. a -> Raw
asRaw = Raw.unsafeFromForeign <<< unsafeToForeign

spec :: Spec Unit
spec =
  let
    check :: forall @a @x. Eq a => Show a => Arbitrary x => Rep a => String -> (x -> a) -> (a -> Raw) -> Spec Unit
    check s xa asRaw_ =
      describe s do
        it "serialize" $ quickCheck \(x :: x) -> (unsafePerformEffect $ runExceptT $ serialize $ xa x) ==? pure (asRaw_ $ xa x)
        it "deserialize" $ quickCheck \(x :: x) -> (unsafePerformEffect $ runExceptT $ deserialize $ asRaw_ $ xa x) ==? pure (xa x)

    check_ :: forall @a. Eq a => Show a => Arbitrary a => Rep a => String -> Spec Unit
    check_ s = check @a @a s identity asRaw

    dateTimeFromArbitrary :: Int -> DateTime
    dateTimeFromArbitrary = Instant.toDateTime <<< unsafePartial fromJust <<< Instant.instant <<< wrap <<< Int.toNumber
  in
    describe "Data.Postgres" do
      check_ @Int "Int"
      check_ @String "String"
      check_ @Boolean "Boolean"
      check_ @Number "Number"
      check_ @Char "Char"

      check @(Maybe String) "Maybe String" identity (maybe null_ asRaw)
      check @(Array String) "Array String" identity asRaw
      check @DateTime "DateTime" dateTimeFromArbitrary (asRaw <<< DateTime.ISO.fromDateTime)

      describe "JSON" do
        describe "Record" do
          it "deserialize" $
            quickCheck \(a /\ b /\ c :: Int /\ String /\ Array {"foo" :: String}) -> unsafePerformEffect do
              let
                obj = {a, b, c}
                json = writeJSON obj
              act :: JSON _ <- smash $ deserialize $ asRaw json
              pure $ obj ==? unwrap act
          it "serialize" $
            quickCheck \(a /\ b /\ c :: Int /\ String /\ Array {"foo" :: String}) -> unsafePerformEffect do
              let obj = {a, b, c}
              act <- smash $ serialize $ JSON obj
              pure $ asRaw (writeJSON obj) ==? act

      describe "Null" do
        it "serialize" $ liftEffect $ shouldEqual null_ =<< (smash $ serialize Null)
        it "deserialize" $ liftEffect $ shouldEqual Null =<< (smash $ deserialize null_)

      describe "Range" do
        it "deserialize" do
          quickCheck \(up /\ lo /\ uinc /\ linc :: Int /\ Int /\ Boolean /\ Boolean) -> unsafePerformEffect do
            let
              record =
                { upper: unsafePerformEffect $ smash $ serialize up
                , lower: unsafePerformEffect $ smash $ serialize lo
                , upperIncl: uinc
                , lowerIncl: linc
                }
              raw = asRaw $ Range.rangeRawFromRecord record
            exp :: Range.Range Int <- smash $ Range.rangeFromRaw record
            act :: Range.Range Int <- smash $ deserialize raw
            pure $ exp ==? act
