module Data.Postgres.Raw where

import Prelude

import Foreign (Foreign)
import Prim.TypeError (class Warn, Text)
import Unsafe.Coerce (unsafeCoerce)

-- | A raw JS value converted from SQL
-- |
-- | In practice, this is an alias for `Foreign` with
-- | type system guarantees that purescript types are
-- | correctly represented in JS to be represented in SQL.
foreign import data Raw :: Type

-- | Stringifies a `Raw` value if a JS primitive,
-- | else returns a debug representation.
-- |
-- | * `{foo: 'bar'} -> "[Object]"`
-- | * `[1, 2, 3] -> "[Array]"`
-- | * `'foo' -> "foo"`
-- | * `123 -> "123"`
foreign import rawToDebugString :: Raw -> String

-- | Performs JS referential equality `===` for primitives
-- | or arrays of primitives, else returns `false`.
foreign import rawDebugEq :: Raw -> Raw -> Boolean

instance Show Raw where
  show = rawToDebugString

instance (Warn (Text "`Eq Raw` only checks equality for JS primitives, and is always `false` for objects.")) => Eq Raw where
  eq = rawDebugEq

-- | Coerce a `Foreign` value to `Raw`.
-- |
-- | This is only safe if the `Foreign` value
-- | is guaranteed to be serializable to a SQL
-- | value via `pg-types`.
unsafeFromForeign :: Foreign -> Raw
unsafeFromForeign = unsafeCoerce

-- | Coerce a `Raw` value to `Foreign`.
asForeign :: Raw -> Foreign
asForeign = unsafeCoerce
