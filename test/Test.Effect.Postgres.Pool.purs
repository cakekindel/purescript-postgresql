module Test.Effect.Postgres.Pool where

import Prelude

import Data.Traversable (traverse)
import Effect.Aff (finally, forkAff, joinFiber)
import Effect.Aff.Postgres.Client as Client
import Effect.Aff.Postgres.Pool as Pool
import Effect.Class (liftEffect)
import Test.Common (config, withPool)
import Test.Event (onceAff)
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (expectError, shouldEqual)

foreign import refEq :: forall a. a -> a -> Boolean

spec :: Spec Unit
spec = describe "Pool" do
  it "make" do
    cfg <- liftEffect config
    void $ liftEffect $ Pool.make cfg
  around withPool do
    it "idleCount, totalCount" \p -> do
      a <- Pool.connect p
      b <- Pool.connect p
      c <- Pool.connect p
      liftEffect $ Pool.release p a
      liftEffect $ Pool.release p b
      finally (liftEffect $ Pool.release p c) do
        Pool.clientIdleCount p `shouldEqual` 2
        Pool.clientCount p `shouldEqual` 3
      Pool.clientIdleCount p `shouldEqual` 3
      Pool.clientCount p `shouldEqual` 3
    it "waitingCount" \p -> do
      a <- Pool.connect p
      b <- Pool.connect p
      c <- Pool.connect p
      dFiber <- forkAff $ Pool.connect p
      let
        rel =
          do
            void $ liftEffect $ traverse (Pool.release p) [ a, b, c ]
            d <- joinFiber dFiber
            liftEffect $ Pool.release p d
      finally rel $ Pool.clientWaitingCount p `shouldEqual` 1
    describe "events" do
      it "connect" \p -> do
        expect <- forkAff $ void $ onceAff Pool.connectE p
        c <- Pool.connect p
        finally (liftEffect $ Pool.release p c) $ joinFiber expect
      it "acquire" \p -> do
        c <- Pool.connect p
        liftEffect $ Pool.release p c
        expect <- forkAff do
          c'' <- onceAff Pool.acquireE p
          refEq c c'' `shouldEqual` true
        c' <- Pool.connect p
        finally (liftEffect $ Pool.release p c') $ joinFiber expect
      it "release" \p -> do
        c <- Pool.connect p
        expect <- forkAff do
          c' <- onceAff Pool.releaseE p
          refEq c c' `shouldEqual` true
        liftEffect $ Pool.release p c
        joinFiber expect
      it "remove" \p -> do
        c <- Pool.connect p
        expect <- forkAff do
          c' <- onceAff Pool.removeE p
          refEq c c' `shouldEqual` true
        liftEffect $ Pool.destroy p c
        joinFiber expect
    it "connect" \p -> do
      c <- Pool.connect p
      let rel = liftEffect $ Pool.release p c
      finally rel $ shouldEqual [ 1 ] =<< Client.query "select 1" c
    describe "destroy" do
      it "throws on query after destroy" \p -> do
        c <- Pool.connect p
        liftEffect $ Pool.destroy p c
        expectError $ Client.exec "select 1" c
      it "different client yielded after destroy" \p -> do
        a <- Pool.connect p
        liftEffect $ Pool.destroy p a
        b <- Pool.connect p
        liftEffect $ Pool.destroy p b
        refEq a b `shouldEqual` false
    describe "release" do
      it "allows reuse" \p -> do
        a <- Pool.connect p
        liftEffect $ Pool.release p a
        b <- Pool.connect p
        liftEffect $ Pool.release p b
        refEq a b `shouldEqual` true
      it "throws when invoked twice" \p -> do
        c <- Pool.connect p
        liftEffect $ Pool.release p c
        expectError $ liftEffect $ Pool.release p c
