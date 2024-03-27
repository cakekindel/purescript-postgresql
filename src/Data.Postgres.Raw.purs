module Data.Postgres.Raw where

import Prelude

import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Raw :: Type
foreign import rawToString :: Raw -> String
foreign import rawEq :: Raw -> Raw -> Boolean

instance Show Raw where
  show = rawToString

instance Eq Raw where
  eq = rawEq

unsafeFromForeign :: Foreign -> Raw
unsafeFromForeign = unsafeCoerce

unsafeToForeign :: Raw -> Foreign
unsafeToForeign = unsafeCoerce
