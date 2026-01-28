module Test.Data.Postgres.Interval where

import Prelude

import Data.Postgres.Interval as Interval
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "Data.Postgres.Interval" do
    it "parse & toRecord" do
      p <- liftEffect $ Interval.parse "3 days 04:05:06"
      Interval.toRecord p `shouldEqual` Interval.zero { days = 3, hours = 4, minutes = 5, seconds = 6 }

    it "make & toRecord" do
      let p = Interval.make $ Interval.zero { days = 3, hours = 4, minutes = 5, seconds = 6 }
      Interval.toRecord p `shouldEqual` Interval.zero { days = 3, hours = 4, minutes = 5, seconds = 6 }

    describe "fromDuration" do
      for_
        [ Milliseconds 100.0 /\ Interval.zero { milliseconds = 100.0 }
        , Milliseconds 1000.0 /\ Interval.zero { seconds = 1 }
        , Milliseconds 1100.0 /\ Interval.zero { seconds = 1, milliseconds = 100.0 }
        , Milliseconds 60000.0 /\ Interval.zero { minutes = 1 }
        , Milliseconds 61100.0 /\ Interval.zero { minutes = 1, seconds = 1, milliseconds = 100.0 }
        , Milliseconds 3600000.0 /\ Interval.zero { hours = 1 }
        , Milliseconds 3661100.0 /\ Interval.zero { hours = 1, minutes = 1, seconds = 1, milliseconds = 100.0 }
        , Milliseconds 86400000.0 /\ Interval.zero { days = 1 }
        , Milliseconds 90061100.0 /\ Interval.zero { days = 1, hours = 1, minutes = 1, seconds = 1, milliseconds = 100.0 }
        ]
        \(i /\ o) -> it ("converts " <> show i) do
          Interval.toRecord (Interval.fromDuration i) `shouldEqual` o
