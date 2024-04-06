module Data.Postgres.Custom.Enum where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftMaybe)
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep as G
import Data.Maybe (Maybe(..))
import Data.Newtype as Newtype
import Data.Postgres (class Rep, RepT, deserialize, serialize)
import Data.Postgres.Custom (quoted)
import Data.Postgres.Query (Query, emptyQuery)
import Data.Postgres.Raw (Raw)
import Data.Symbol (class IsSymbol)
import Foreign (ForeignError(..))
import Type.Prelude (Proxy(..), reflectSymbol)

typeName :: forall @a ty. CustomEnum a ty => String
typeName = reflectSymbol (Proxy @ty)

class (IsSymbol ty, Rep a) <= CustomEnum a ty | a -> ty where
  enumVariants :: NonEmptyArray a
  parseEnum :: String -> Maybe a
  printEnum :: a -> String

class GenericCustomEnum a where
  genericEnumVariants' :: NonEmptyArray a
  genericParseEnum' :: String -> Maybe a
  genericPrintEnum' :: a -> String

instance IsSymbol n => GenericCustomEnum (G.Constructor n G.NoArguments) where
  genericEnumVariants' = pure (G.Constructor @n G.NoArguments)
  genericParseEnum' s
    | s == reflectSymbol (Proxy @n) = Just (G.Constructor @n G.NoArguments)
    | otherwise = Nothing
  genericPrintEnum' _ = reflectSymbol (Proxy @n)

instance (GenericCustomEnum a, GenericCustomEnum b) => GenericCustomEnum (G.Sum a b) where
  genericEnumVariants' = (G.Inl <$> genericEnumVariants' @a) <> (G.Inr <$> genericEnumVariants' @b)
  genericParseEnum' s = (G.Inl <$> genericParseEnum' @a s) <|> (G.Inr <$> genericParseEnum' @b s)
  genericPrintEnum' (G.Inl a) = genericPrintEnum' a
  genericPrintEnum' (G.Inr a) = genericPrintEnum' a

enumDeserialize :: forall @a ty. CustomEnum a ty => Raw -> RepT a
enumDeserialize raw = do
  s <- deserialize raw
  let e = pure $ ForeignError $ "unsupported enum variant for " <> typeName @a <> ": " <> quoted s
  liftMaybe e $ parseEnum s

enumSerialize :: forall @a ty. CustomEnum a ty => a -> RepT Raw
enumSerialize = serialize <<< printEnum

enumPrintExpr :: forall @a ty. CustomEnum a ty => a -> Maybe String
enumPrintExpr = Just <<< (\s -> quoted s <> " :: " <> typeName @a) <<< printEnum

genericEnumVariants :: forall a g. Generic a g => GenericCustomEnum g => NonEmptyArray a
genericEnumVariants = G.to <$> genericEnumVariants'

genericParseEnum :: forall a g. Generic a g => GenericCustomEnum g => String -> Maybe a
genericParseEnum = map G.to <<< genericParseEnum'

genericPrintEnum :: forall a g. Generic a g => GenericCustomEnum g => a -> String
genericPrintEnum = genericPrintEnum' <<< G.from

create :: forall @a ty. CustomEnum a ty => Query
create =
  let
    variants' :: NonEmptyArray a
    variants' = enumVariants
    variants = intercalate ", " $ quoted <$> printEnum <$> variants'
    q = "create type " <> typeName @a <> " as enum (" <> variants <> ");"
  in
    Newtype.modify (_ { text = q }) emptyQuery
