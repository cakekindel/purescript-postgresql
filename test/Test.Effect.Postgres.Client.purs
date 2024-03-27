module Test.Effect.Postgres.Client where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Array as Array
import Data.Either (isLeft)
import Data.Postgres (deserialize, smash)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Aff.Postgres.Client as PG.Aff.Client
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Postgres.Client as PG
import Effect.Postgres.Client as PG.Client
import Effect.Postgres.Result as Result
import Node.Path as Path
import Node.Process (cwd)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

client :: Aff PG.Client
client = do
  cwd' <- liftEffect cwd
  host <- liftEffect $ Path.resolve [ cwd' ] "./pg"
  liftEffect $ PG.Client.make { host, user: "postgres", password: "password", database: "postgres" }

spec :: Spec Unit
spec = do
  describe "Client" do
    describe "make" do
      it "does not throw" $ void $ client
    describe "connect" do
      it "does not throw" $ PG.Aff.Client.connect =<< client
    describe "end" do
      it "does not throw" $ do
        c <- client
        PG.Aff.Client.connect c
        PG.Aff.Client.end c
    describe "query" do
      it "ok if connected" $ do
        c <- client
        PG.Aff.Client.connect c
        res <- Result.rows <$> PG.Aff.Client.query "select unnest(array[1, 2, 3])" c
        ints :: Array Int <- liftEffect $ smash $ traverse deserialize $ Array.catMaybes $ map Array.head res
        ints `shouldEqual` [ 1, 2, 3 ]
      it "throws if ended" $ do
        c <- client
        PG.Aff.Client.connect c
        PG.Aff.Client.end c
        res <- try $ PG.Aff.Client.query "select 1" c
        isLeft res `shouldEqual` true
