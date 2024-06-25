module Test.Control.Monad.Postgres where

import Prelude

import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (catchError, throwError, try)
import Control.Monad.Fork.Class (class MonadBracket, bracket)
import Control.Monad.Postgres (PostgresT, cursor, exec_, fetch, fetchAll, fetchOne, query, runPostgres, transaction)
import Control.Monad.Reader (ask)
import Control.Parallel (parTraverse)
import Data.Array as Array
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flag
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Fiber)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Unlift (UnliftAff(..), askUnliftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error, error)
import Effect.Postgres.Error as E
import Effect.Postgres.Error.Except as X
import Effect.Postgres.Error.RE as RE
import Partial.Unsafe (unsafePartial)
import Test.Common (re, withConfig)
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (shouldEqual)

withTable :: forall a m. MonadBracket Error Fiber m => MonadAff m => String -> PostgresT m a -> PostgresT m a
withTable s m =
  let
    tabname = unsafePartial fromJust $ join $ Array.index (maybe [] Array.NonEmpty.toArray $ Regex.match (re "create table (\\w+)" Regex.Flag.ignoreCase) s) 1
  in
    bracket (exec_ s) (\_ _ -> exec_ $ "drop table " <> tabname <> ";") (const m)

spec :: Spec Unit
spec =
  around withConfig $ describe "Control.Monad.Postgres" do
    it "empty works" \cfg -> X.run $ runPostgres cfg $ pure unit
    it "connects" \cfg -> shouldEqual 1 =<< X.run (runPostgres cfg $ query "select 1")
    it "multiple sessions serially" \cfg -> do
      a /\ b <- X.run $ runPostgres cfg do
        a <- query "select 1"
        b <- query "select 2"
        pure $ a /\ b
      a `shouldEqual` 1
      b `shouldEqual` 2
    it "multiple sessions concurrently" \cfg -> do
      nums <- X.run $ runPostgres cfg $ parTraverse (\n -> query $ "select $1 :: int" /\ n) (Array.range 1 3)
      Array.sort nums `shouldEqual` [1, 2, 3]
    it "transaction commits" \cfg -> do
      a <- X.run $ runPostgres cfg do
        exec_ "create temporary table test_txn_commits (id int);"
        transaction $ exec_ "insert into test_txn_commits values (1);"
        query "select * from test_txn_commits"
      a `shouldEqual` [ 1 ]
    it "transaction rolls back" \cfg -> do
      a <- X.run $ runPostgres cfg do
        exec_ "create temporary table test_txn_rolls_back (id int);"
        exec_ "insert into test_txn_rolls_back values (1);"
        void $ try $ transaction do
          exec_ "insert into test_txn_rolls_back values (2);"
          throwError $ pure $ E.Other $ error "foo"
        query "select * from test_txn_rolls_back"
      a `shouldEqual` [1]
    it "cursor" \cfg ->
      X.run $ runPostgres cfg do
        exec_ $ "create temporary table test_cursor_data (id int primary key generated always as identity)"
        for_ (Array.range 1 50) $ const $ exec_ "insert into test_cursor_data (id) values (default);"
        cursor @Int "test_cursor" "select id from test_cursor_data" do
          UnliftAff unliftAff <- askUnliftAff
          liftAff $ shouldEqual (Just 1) =<< unliftAff fetchOne
          liftAff $ shouldEqual (Just 2) =<< unliftAff fetchOne
          liftAff $ shouldEqual (Just 3) =<< unliftAff fetchOne
          liftAff $ shouldEqual [ 4, 5, 6, 7, 8 ] =<< unliftAff (fetch 5)
          liftAff $ shouldEqual (Array.range 9 50) =<< unliftAff fetchAll
