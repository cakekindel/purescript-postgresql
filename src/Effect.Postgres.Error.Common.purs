module Effect.Postgres.Error.Common where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic)
import Data.Postgres.Query (Query)
import Data.Show.Generic (genericShow)
import Effect.Exception as Effect
import Foreign (MultipleErrors)

type E = NonEmptyArray Error

data Error
  = Deserializing Query MultipleErrors
  | Serializing MultipleErrors
  | Executing Query Effect.Error
  | Connecting Effect.Error
  | Disconnecting Effect.Error
  | Other Effect.Error

derive instance Generic Error _
instance Show Error where
  show = genericShow

toException' :: Error -> Effect.Error
toException' = Effect.error <<< show

toException :: E -> Effect.Error
toException = Effect.error <<< show
