module Test.Effect.Postgres.Client where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Either (Either, isLeft)
import Data.Newtype (wrap)
import Data.Postgres (JSON(..))
import Data.PreciseDateTime (fromRFC3339String, toDateTimeLossy)
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Effect.Aff.Postgres.Client (query)
import Effect.Aff.Postgres.Client as PG.Aff.Client
import Effect.Class (liftEffect)
import Effect.Postgres.Client as PG
import Node.Path as Path
import Node.Process (cwd)
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (shouldEqual)

config
  :: Effect
       { database :: String
       , host :: String
       , password :: String
       , user :: String
       }
config = do
  cwd' <- liftEffect cwd
  host <- liftEffect $ Path.resolve [ cwd' ] "./pg"
  pure { host, user: "postgres", password: "password", database: "postgres" }

withClient :: (PG.Client -> Aff Unit) -> Aff Unit
withClient = bracket (PG.Aff.Client.connected =<< liftEffect config) PG.Aff.Client.end

spec :: Spec Unit
spec =
  around withClient do
    describe "Client" do
      it "connect & end do not throw" $ const $ pure unit
      describe "query" do
        it "ok if connected" \c -> shouldEqual [ 1, 2, 3 ] =<< query "select unnest(array[1, 2, 3])" c
        it "throws if ended" \c -> do
          PG.Aff.Client.end c
          res :: Either _ (Array Int) <- try $ query "select 1" c
          isLeft res `shouldEqual` true
        it "rowsAffected is correct" \c -> do
          void $ PG.Aff.Client.exec "create temp table foo (bar int);" c
          cta <- PG.Aff.Client.exec "insert into foo values (1);" c
          cta `shouldEqual` 1
          ctb <- PG.Aff.Client.exec "insert into foo values (1), (2), (3);" c
          ctb `shouldEqual` 3
          ctc <- PG.Aff.Client.exec "update foo set bar = 10;" c
          ctc `shouldEqual` 4
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
