module Test.Effect.Postgres.Client where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Postgres (JSON(..))
import Data.PreciseDateTime (fromRFC3339String, toDateTimeLossy)
import Effect (Effect)
import Effect.Aff (Aff, forkAff, joinFiber, makeAff)
import Effect.Aff.Postgres.Client (query)
import Effect.Aff.Postgres.Client as Client
import Effect.Exception as Error
import Effect.Uncurried (EffectFn1)
import Node.EventEmitter (EventHandle, once)
import Test.Common (withClient)
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  around withClient do
    describe "Client" do
      describe "events" do
        let
          once_ :: forall a b. EventHandle Client.Client (a -> Effect Unit) (EffectFn1 b Unit) -> Client.Client -> Aff a
          once_ e c = makeAff \res -> mempty <$ once e (res <<< Right) c
        it "end" \c -> do
          endEvent <- forkAff $ makeAff \res -> mempty <$ once Client.endE (res $ Right unit) c
          Client.end c
          void $ joinFiber endEvent
        it "notice" \c -> do
          noticeEvent <- forkAff $ once_ Client.noticeE c
          void $ Client.exec "do language plpgsql $$ begin raise notice 'hello'; end; $$;" c
          e <- joinFiber noticeEvent
          Error.message e `shouldEqual` "hello"
        it "notification" \c -> do
          void $ Client.exec "listen hello;" c
          notifEvent <- forkAff $ once_ Client.notificationE c
          void $ Client.exec "notify hello, 'world';" c
          n <- joinFiber notifEvent
          n.payload `shouldEqual` (Just "world")
      it "connect & end do not throw" $ const $ pure unit
      describe "query" do
        it "ok if connected" \c -> shouldEqual [ 1, 2, 3 ] =<< query "select unnest(array[1, 2, 3])" c
        it "throws if ended" \c -> do
          Client.end c
          res :: Either _ (Array Int) <- try $ query "select 1" c
          isLeft res `shouldEqual` true
        it "rowsAffected is correct" \c -> do
          void $ Client.exec "create temp table foo (bar int);" c
          shouldEqual 1 =<< Client.exec "insert into foo values (1);" c
          shouldEqual 3 =<< Client.exec "insert into foo values (1), (2), (3);" c
          shouldEqual 4 =<< Client.exec "update foo set bar = 10;" c
        describe "timestamp" do
          it "unmarshals" \c -> do
            let exp = toDateTimeLossy <$> fromRFC3339String (wrap "2020-01-01T00:00:00Z")
            shouldEqual [ exp ] =<< query "select '2020-01-01T00:00:00Z' :: timestamptz" c
          it "is string" \c -> shouldEqual [ "2020-01-01 00:00:00+00" ] =<< query "select '2020-01-01T00:00:00Z' :: timestamptz" c
          it "array is string" \c -> shouldEqual [ [ "2020-01-01 00:00:00+00" ] ] =<< query "select array['2020-01-01T00:00:00Z' :: timestamptz]" c
        describe "json" do
          it "unmarshals" \c -> shouldEqual [ JSON { foo: "bar" } ] =<< query "select '{\"foo\": \"bar\"}' :: json" c
          it "is string" \c -> shouldEqual [ "{\"foo\": \"bar\"}" ] =<< query "select '{\"foo\": \"bar\"}' :: json" c
          it "array is string" \c -> shouldEqual [ [ "{\"foo\": \"bar\"}" ] ] =<< query "select array['{\"foo\": \"bar\"}' :: json]" c
