module Test.Effect.Postgres.Client where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Either (Either, isLeft)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Postgres (JSON(..))
import Data.PreciseDateTime (fromRFC3339String, toDateTimeLossy)
import Effect.Aff (forkAff, joinFiber)
import Effect.Aff.Postgres.Client (query)
import Effect.Aff.Postgres.Client as Client
import Effect.Exception as Error
import Test.Common (onceAff, withClient)
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  around withClient do
    describe "Client" do
      describe "events" do
        it "end" \c -> do
          expect <- forkAff $ onceAff Client.endE c
          Client.end c
          joinFiber expect
        it "notice" \c -> do
          expect <- forkAff do
            e <- onceAff Client.noticeE c
            Error.message e `shouldEqual` "hello"
          void $ Client.exec "do language plpgsql $$ begin raise notice 'hello'; end; $$;" c
          joinFiber expect
        it "notification" \c -> do
          void $ Client.exec "listen hello;" c
          expect <- forkAff do
            n <- onceAff Client.notificationE c
            n.payload `shouldEqual` (Just "world")
          void $ Client.exec "notify hello, 'world';" c
          joinFiber expect
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
            shouldEqual exp =<< query "select '2020-01-01T00:00:00Z' :: timestamptz" c
          it "is string" \c -> shouldEqual "2020-01-01 00:00:00+00" =<< query "select '2020-01-01T00:00:00Z' :: timestamptz" c
          it "array is string" \c -> shouldEqual [ [ "2020-01-01 00:00:00+00" ] ] =<< query "select array['2020-01-01T00:00:00Z' :: timestamptz]" c
        describe "json" do
          it "unmarshals" \c -> shouldEqual (JSON { foo: "bar" }) =<< query "select '{\"foo\": \"bar\"}' :: json" c
          it "is string" \c -> shouldEqual "{\"foo\": \"bar\"}" =<< query "select '{\"foo\": \"bar\"}' :: json" c
          it "array is string" \c -> shouldEqual [ [ "{\"foo\": \"bar\"}" ] ] =<< query "select array['{\"foo\": \"bar\"}' :: json]" c
