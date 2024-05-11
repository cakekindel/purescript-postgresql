module Data.Postgres.Custom where

import Prelude

import Data.Either (hush)
import Data.Maybe (fromJust)
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Partial.Unsafe (unsafePartial)

quoted :: String -> String
quoted s = "'" <> s <> "'"

upperRe :: Regex
upperRe = unsafePartial fromJust $ hush $ Regex.regex "[A-Z]" Regex.Flags.global

leadingUnderRe :: Regex
leadingUnderRe = unsafePartial fromJust $ hush $ Regex.regex "^_" Regex.Flags.noFlags

pascalToSnake :: String -> String
pascalToSnake = String.toLower <<< Regex.replace leadingUnderRe "" <<< Regex.replace upperRe "_$1"
