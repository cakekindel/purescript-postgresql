module Test.Data.Postgres.Custom where

import Prelude

import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (runExceptT)
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Postgres (deserialize, serialize, smash)
import Data.Postgres.Custom (class CustomDeserialize, class CustomSerialize, customDeserialize)
import Data.Postgres.Custom.Enum (class CustomEnum, create, enumDeserialize, enumPrintExpr, enumSerialize, genericEnumVariants, genericParseEnum, genericPrintEnum, parseEnum, printEnum)
import Data.Show.Generic (genericShow)
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Unsafe.Coerce (unsafeCoerce)

data Enum1 = E1A

derive instance Generic Enum1 _
derive instance Eq Enum1
instance Show Enum1 where
  show = genericShow

instance CustomSerialize Enum1 "enum_1" where
  customPrintExpr a = enumPrintExpr a
  customSerialize a = enumSerialize a

instance CustomDeserialize Enum1 "enum_1" where
  customDeserialize a = enumDeserialize a

instance CustomEnum Enum1 "enum_1" where
  printEnum = genericPrintEnum
  parseEnum = genericParseEnum
  enumVariants = genericEnumVariants

data Enum2 = E2A | E2B

derive instance Generic Enum2 _
derive instance Eq Enum2
instance Show Enum2 where
  show = genericShow

instance CustomSerialize Enum2 "enum_2" where
  customPrintExpr a = enumPrintExpr a
  customSerialize a = enumSerialize a

instance CustomDeserialize Enum2 "enum_2" where
  customDeserialize a = enumDeserialize a

instance CustomEnum Enum2 "enum_2" where
  printEnum a = genericPrintEnum a
  parseEnum a = genericParseEnum a
  enumVariants = genericEnumVariants

data Enum5 = E5A | E5B | E5C | E5D | E5E

derive instance Generic Enum5 _
derive instance Eq Enum5
instance Show Enum5 where
  show = genericShow

instance CustomSerialize Enum5 "enum_5" where
  customPrintExpr a = enumPrintExpr a
  customSerialize a = enumSerialize a

instance CustomDeserialize Enum5 "enum_5" where
  customDeserialize a = enumDeserialize a

instance CustomEnum Enum5 "enum_5" where
  printEnum a = genericPrintEnum a
  parseEnum a = genericParseEnum a
  enumVariants = genericEnumVariants

spec :: Spec Unit
spec =
  describe "Test.Data.Postgres.Custom" do
    describe "Enum" do
      it "serialize" do
        act <- liftEffect $ smash $ serialize E5A
        exp <- liftEffect $ smash $ serialize "E5A"
        act `shouldEqual` exp
      it "deserialize" do
        act <- liftEffect $ smash $ deserialize $ unsafeCoerce "E5A"
        act `shouldEqual` E5A
      it "create" do
        (_.text $ unwrap $ create @Enum1) `shouldEqual` "create type enum_1 as enum ('E1A');"
        (_.text $ unwrap $ create @Enum2) `shouldEqual` "create type enum_2 as enum ('E2A', 'E2B');"
        (_.text $ unwrap $ create @Enum5) `shouldEqual` "create type enum_5 as enum ('E5A', 'E5B', 'E5C', 'E5D', 'E5E');"
      it "parseEnum" do
        parseEnum "E1A" `shouldEqual` Just E1A
        parseEnum "E2A" `shouldEqual` Just E2A
        parseEnum "E2B" `shouldEqual` Just E2B
        parseEnum "E5B" `shouldEqual` Just E5B
      it "printEnum" do
        printEnum E1A `shouldEqual` "E1A"
        printEnum E2A `shouldEqual` "E2A"
        printEnum E2B `shouldEqual` "E2B"
        printEnum E5D `shouldEqual` "E5D"
      it "enumPrintExpr" do
        enumPrintExpr E1A `shouldEqual` Just "'E1A' :: enum_1"
        enumPrintExpr E2A `shouldEqual` Just "'E2A' :: enum_2"
        enumPrintExpr E2B `shouldEqual` Just "'E2B' :: enum_2"
        enumPrintExpr E5D `shouldEqual` Just "'E5D' :: enum_5"
