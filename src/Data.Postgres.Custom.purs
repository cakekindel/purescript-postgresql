module Data.Postgres.Custom where

import Prelude

import Control.Monad.Except (ExceptT)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Postgres.Raw (Raw)
import Effect (Effect)
import Foreign (ForeignError)
import Type.Data.Symbol (reflectSymbol)
import Type.Prelude (class IsSymbol, Proxy(..))

class (IsSymbol ty) <= CustomSerialize a ty | a -> ty where
  customPrintExpr :: a -> Maybe String
  customSerialize :: a -> ExceptT (NonEmptyList ForeignError) Effect Raw

class (IsSymbol ty) <= CustomDeserialize a ty | a -> ty where
  customDeserialize :: Raw -> ExceptT (NonEmptyList ForeignError) Effect a

class (IsSymbol ty, CustomSerialize a ty, CustomDeserialize a ty) <= CustomRep a ty | a -> ty

instance (IsSymbol ty, CustomSerialize a ty, CustomDeserialize a ty) => CustomRep a ty

quoted :: String -> String
quoted s = "'" <> s <> "'"

typeName :: forall @a ty. CustomRep a ty => String
typeName = reflectSymbol (Proxy @ty)
