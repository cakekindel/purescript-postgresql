module Node.FS.PinnedVersion where

import Prelude

import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS

foo :: Aff Unit
foo = FS.writeTextFile UTF8 "foo" "foo"
