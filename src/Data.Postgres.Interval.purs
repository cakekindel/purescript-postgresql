module Data.Postgres.Interval where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (class Duration, Days(..), Hours(..), Milliseconds(..), Minutes(..), Seconds(..), convertDuration)
import Effect (Effect)

zero :: IntervalRecord
zero = {years: 0, months: 0, days: 0, hours: 0, minutes: 0, seconds: 0, milliseconds: 0.0}

type IntervalRecord =
  { years :: Int
  , months :: Int
  , days :: Int
  , hours :: Int
  , minutes :: Int
  , seconds :: Int
  , milliseconds :: Number
  }

foreign import data Interval :: Type

foreign import make :: IntervalRecord -> Interval
foreign import parse :: String -> Effect Interval

foreign import getYears :: Interval -> Int
foreign import getMonths :: Interval -> Int
foreign import getDays :: Interval -> Int
foreign import getHours :: Interval -> Int
foreign import getMinutes :: Interval -> Int
foreign import getSeconds :: Interval -> Int
foreign import getMilliseconds :: Interval -> Number

toDuration :: forall d. Semigroup d => Duration d => Interval -> Maybe d
toDuration a =
  let
    includesMonths = getYears a > 0 || getMonths a > 0

    days :: d
    days = convertDuration $ Days $ Int.toNumber $ getDays a

    hours :: d
    hours = convertDuration $ Hours $ Int.toNumber $ getHours a

    minutes :: d
    minutes = convertDuration $ Minutes $ Int.toNumber $ getMinutes a

    seconds :: d
    seconds = convertDuration $ Seconds $ Int.toNumber $ getSeconds a

    milliseconds :: d
    milliseconds = convertDuration $ Milliseconds $ getMilliseconds a
  in
    if includesMonths then Nothing else Just (days <> hours <> minutes <> seconds <> milliseconds)

toRecord :: Interval -> IntervalRecord
toRecord a =
  { years: getYears a
  , months: getMonths a
  , days: getDays a
  , hours: getHours a
  , minutes: getMinutes a
  , seconds: getSeconds a
  , milliseconds: getMilliseconds a
  }

fromDuration :: forall d. Duration d => d -> Interval
fromDuration a =
  let
    millisTotal :: Number
    millisTotal = (unwrap :: Milliseconds -> Number) $ convertDuration a
    secondFactor = 1000.0
    minuteFactor = 60.0 * secondFactor
    hourFactor = 60.0 * minuteFactor
    dayFactor = 24.0 * hourFactor
    days = Int.trunc $ millisTotal / dayFactor
    daysRem = millisTotal - (Int.toNumber days * dayFactor)
    hours = Int.trunc $ daysRem / hourFactor
    hoursRem = daysRem - (Int.toNumber hours * hourFactor)
    minutes = Int.trunc $ hoursRem / minuteFactor
    minutesRem = hoursRem - (Int.toNumber minutes * minuteFactor)
    seconds = Int.trunc $ minutesRem / secondFactor
    milliseconds = minutesRem - (Int.toNumber seconds * secondFactor)
  in
    make {years: 0, months: 0, days, hours, minutes, seconds, milliseconds}
