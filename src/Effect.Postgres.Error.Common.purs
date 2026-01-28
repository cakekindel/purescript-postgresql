module Effect.Postgres.Error.Common where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Maybe (maybe)
import Data.Newtype (wrap)
import Data.Postgres.Query (Query)
import Data.String as String
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
  show = toString

toString :: Error -> String
toString =
  let
    indent n s = fold $ ((fold $ Array.replicate n "  ") <> _) <$> String.split (wrap "\n") s
    jsError n e =
      indent n (Effect.message e)
        <> maybe "" (\s -> "\n" <> indent n s) (Effect.stack e)
  in
    case _ of
      Deserializing q es -> "Deserializing " <> show q <> "\n" <> indent 1 (show es)
      Serializing es -> "Serializing" <> "\n" <> indent 1 (show es)
      Executing q e -> "Executing " <> show q <> "\n" <> jsError 1 e
      Connecting e -> "Connecting\n" <> jsError 1 e
      Disconnecting e -> "Disconnecting\n" <> jsError 1 e
      Other e -> "Other\n" <> jsError 1 e

toException' :: Error -> Effect.Error
toException' = Effect.error <<< show

toException :: E -> Effect.Error
toException = Effect.error <<< show
