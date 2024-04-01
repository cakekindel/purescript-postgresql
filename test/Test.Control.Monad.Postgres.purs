module Test.Control.Monad.Postgres where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Fork.Class (class MonadBracket, bracket)
import Control.Monad.Postgres (PostgresT, exec_, query, runPostgres, session, transaction)
import Data.Array as Array
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Maybe (fromJust, maybe)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flag
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
    bracket (session $ exec_ s) (\_ _ -> session $ exec_ $ "drop table " <> tabname <> ";") (const m)

spec :: Spec Unit
spec =
  around withConfig $ describe "Control.Monad.Postgres" do
    it "empty works" \cfg -> runPostgres cfg $ pure unit
    it "connects" \cfg -> runPostgres cfg do
      act <- session $ query "select 1"
      act `shouldEqual` 1
    it "connects multiple" \cfg -> runPostgres cfg do
      a <- session $ query "select 1"
      b <- session $ query "select 2"
      a `shouldEqual` 1
      b `shouldEqual` 2
    it "transaction commits" \cfg -> runPostgres cfg do
      withTable "create table test_txn_commits (id int);" do
        transaction $ exec_ "insert into test_txn_commits values (1);"
        act <- session $ query "select * from test_txn_commits"
        act `shouldEqual` [ 1 ]
    it "transaction rolls back" \cfg -> runPostgres cfg do
      withTable "create table test_txn_rolls_back (id int);" do
        expectError $ transaction do
          exec_ "insert into test_txn_rolls_back values (1);"
          throwError $ error "foo"
        act :: Array Int <- session $ query "select * from test_txn_rolls_back"
        act `shouldEqual` []
