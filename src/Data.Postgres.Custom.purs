module Data.Postgres.Custom where

import Prelude

import Control.Monad.Except (ExceptT)
import Data.Either (hush)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe, fromJust)
import Data.Postgres.Raw (Raw)
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Effect (Effect)
import Foreign (ForeignError)
import Partial.Unsafe (unsafePartial)
import Type.Data.Symbol (reflectSymbol)
import Type.Prelude (class IsSymbol, Proxy(..))

quoted :: String -> String
quoted s = "'" <> s <> "'"

upperRe :: Regex
upperRe = unsafePartial fromJust $ hush $ Regex.regex "[A-Z]" Regex.Flags.global

leadingUnderRe :: Regex
leadingUnderRe = unsafePartial fromJust $ hush $ Regex.regex "^_" Regex.Flags.noFlags

pascalToSnake :: String -> String
pascalToSnake = String.toLower <<< Regex.replace leadingUnderRe "" <<< Regex.replace upperRe "_$1"
