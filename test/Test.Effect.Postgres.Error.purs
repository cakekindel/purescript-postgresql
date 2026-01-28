module Test.Effect.Postgres.Error where

import Prelude hiding (join)

import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Fork.Class (bracket, fork, join)
import Control.Parallel (parOneOf, parSequence)
import Data.Either (isLeft)
import Data.Newtype (wrap)
import Data.Postgres (deserialize)
import Data.Postgres.Query (stringQuery)
import Data.Postgres.Raw (Raw)
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception as Exn
import Effect.Postgres.Error (Error(..))
import Effect.Postgres.Error.Except as E
import Effect.Postgres.Error.RE (RE)
import Effect.Postgres.Error.RE as RE
import Test.Common (withPoolClient)
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Unsafe.Coerce (unsafeCoerce)

spec :: Spec Unit
spec =
  around withPoolClient $ describe "Effect.Postgres.Error" do
    describe "Except" do
      it "catches Aff" $ const do
        either <- E.toEither $ E.exception (throwError $ Exn.error "foo")
        isLeft either `shouldEqual` true
      it "catches Effect" $ const do
        either <- liftEffect $ E.toEither $ E.exception (throwError $ Exn.error "foo")
        isLeft either `shouldEqual` true
      it "catches RepT" $ const do
        let
          parse = deserialize @Int (unsafeCoerce "foo" :: Raw)
        either <- liftEffect $ E.toEither $ E.parsing (stringQuery "select 'foo'") parse
        isLeft either `shouldEqual` true
      it "catches Effect exception in RepT" $ const do
        let
          parse = lift $ throwError $ Exn.error "foo"
        either <- liftEffect $ E.toEither $ E.parsing (stringQuery "select 'foo'") parse
        isLeft either `shouldEqual` true
    describe "RE" do
      it "liftAff catches exceptions" $ const do
        either <- RE.toEither (liftAff $ throwError $ Exn.error "foo") unit
        isLeft either `shouldEqual` true
      it "liftEffect catches exceptions" $ const do
        either <- RE.toEither (liftEffect $ throwError $ Exn.error "foo") unit
        isLeft either `shouldEqual` true
      it "liftExcept catches exceptions" $ const do
        either <- RE.toEither (throwError $ pure $ Other $ Exn.error "foo") unit
        isLeft either `shouldEqual` true
      it "fork > join catches Fiber exceptions" $ const do
        either <- flip RE.toEither unit do
          fiber <- fork (liftAff $ throwError $ Exn.error "foo") :: RE Unit Aff _
          liftAff $ delay $ wrap 1.0
          join fiber
        isLeft either `shouldEqual` true
      it "bracket catches error in acq" $ const do
        either <-
          flip RE.toEither unit
            $ bracket
                (liftAff $ throwError $ Exn.error "foo")
                (const $ const $ pure unit)
                (const $ pure unit)
        isLeft either `shouldEqual` true
      it "bracket catches error in rel" $ const do
        either <-
          flip RE.toEither unit
            $ bracket
                (pure unit)
                (const $ const $ liftAff $ throwError $ Exn.error "foo")
                (const $ pure unit)
        isLeft either `shouldEqual` true
      it "bracket catches error in go" $ const do
        either <-
          flip RE.toEither unit
            $ bracket
                (pure unit)
                (const $ const $ pure unit)
                (const $ liftAff $ throwError $ Exn.error "foo")
        isLeft either `shouldEqual` true
      it "forked bracket catches error in acq" $ const do
        either <- flip RE.toEither unit do
          fiber <-
            fork
              $ bracket
                  (liftAff $ throwError $ Exn.error "foo")
                  (const $ const $ pure unit)
                  (const $ pure unit)
          liftAff $ delay $ wrap 1.0
          join fiber
        isLeft either `shouldEqual` true
      it "forked bracket catches error in rel" $ const do
        either <- flip RE.toEither unit do
          fiber <-
            fork
              $ bracket
                  (pure unit)
                  (const $ const $ liftAff $ throwError $ Exn.error "foo")
                  (const $ pure unit)
          liftAff $ delay $ wrap 1.0
          join fiber
        isLeft either `shouldEqual` true
      it "forked bracket catches error in go" $ const do
        either <- flip RE.toEither unit do
          fiber <-
            fork
              $ bracket
                  (pure unit)
                  (const $ const $ pure unit)
                  (const $ liftAff $ throwError $ Exn.error "foo")
          liftAff $ delay $ wrap 1.0
          join fiber
        isLeft either `shouldEqual` true
      it "catches errors in `parSequence`" $ const do
        either <-
          flip RE.toEither unit
            $ parSequence
            $
              [ liftAff $ throwError $ Exn.error "a"
              , pure "a"
              ]
        isLeft either `shouldEqual` true
      it "catches errors in `parOneOf`" $ const do
        either <-
          flip RE.toEither unit
            $ parOneOf
            $
              [ liftAff $ throwError $ Exn.error "a"
              , liftAff $ throwError $ Exn.error "b"
              ]
        isLeft either `shouldEqual` true
