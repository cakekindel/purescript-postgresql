module Effect.Postgres.Result where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Postgres.Raw (Raw)

foreign import data Result :: Type

foreign import rowsAffectedImpl :: Result -> Nullable Number
foreign import rows :: Result -> Array (Array Raw)

rowsAffected :: Result -> Maybe Int
rowsAffected = Int.fromNumber <=< Nullable.toMaybe <<< rowsAffectedImpl
