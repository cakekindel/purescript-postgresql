module Test.Control.Monad.Postgres where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Fork.Class (class MonadBracket, bracket)
import Control.Monad.Postgres (PostgresT, exec_, query, runPostgres, session, transaction, cursor, fetch, fetchAll, fetchOne)
import Control.Parallel (parTraverse_)
import Data.Array as Array
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flag
import Data.Traversable (for_)
import Effect.Aff (Fiber)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error, error)
import Partial.Unsafe (unsafePartial)
import Test.Common (re, withConfig)
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (expectError, shouldEqual)

withTable :: forall a m. MonadBracket Error Fiber m => MonadAff m => String -> PostgresT m a -> PostgresT m a
withTable s m =
  let
    tabname = unsafePartial fromJust $ join $ Array.index (maybe [] Array.NonEmpty.toArray $ Regex.match (re "create table (\\w+)" Regex.Flag.ignoreCase) s) 1
  in
    bracket (exec_ s) (\_ _ -> exec_ $ "drop table " <> tabname <> ";") (const m)

spec :: Spec Unit
spec =
  around withConfig $ describe "Control.Monad.Postgres" do
    it "empty works" \cfg -> runPostgres cfg $ pure unit
    it "connects" \cfg -> runPostgres cfg $ shouldEqual 1 =<< query "select 1"
    it "multiple sessions serially" \cfg -> runPostgres cfg do
      shouldEqual 1 =<< query "select 1"
      shouldEqual 2 =<< query "select 2"
    it "multiple sessions concurrently" \cfg -> runPostgres cfg do
      flip parTraverse_ [ 1, 2, 3 ] \_ -> shouldEqual 1 =<< query "select 1"
    it "transaction commits" \cfg -> runPostgres cfg do
      exec_ "create temporary table test_txn_commits (id int);"
      transaction $ exec_ "insert into test_txn_commits values (1);"
      shouldEqual [ 1 ] =<< query "select * from test_txn_commits"
    it "transaction rolls back" \cfg -> runPostgres cfg do
      exec_ "create temporary table test_txn_rolls_back (id int);"
      exec_ "insert into test_txn_rolls_back values (1);"
      expectError $ transaction do
        exec_ "insert into test_txn_rolls_back values (2);"
        throwError $ error "foo"
      shouldEqual [ 1 ] =<< query "select * from test_txn_rolls_back"
    it "cursor" \cfg -> runPostgres cfg do
      exec_ $ "create temporary table test_cursor_data (id int primary key generated always as identity)"
      for_ (Array.range 1 50) $ const $ exec_ "insert into test_cursor_data (id) values (default);"
      cursor @Int "test_cursor" "select id from test_cursor_data" do
        shouldEqual (Just 1) =<< fetchOne
        shouldEqual (Just 2) =<< fetchOne
        shouldEqual (Just 3) =<< fetchOne
        shouldEqual [ 4, 5, 6, 7, 8 ] =<< fetch 5
        shouldEqual (Array.range 9 50) =<< fetchAll
