module Data.Postgres.Query.Builder where

import Prelude

import Control.Monad.State (StateT, get, modify, runStateT)
import Data.Array as Array
import Data.Newtype (unwrap)
import Data.Newtype as Newtype
import Data.Postgres (class Rep, serialize, smash)
import Data.Postgres.Query (Query, emptyQuery)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Record as Record
import Type.Prelude (Proxy(..))

-- | Monad for building parameterized queries without managing positional
-- | parameters directly
-- |
-- | For example, given the table `CREATE TABLE foo (id INT NOT NULL PRIMARY KEY, bar TEXT NOT NULL)`
-- |
-- | ```purescript
-- | updateFoo :: Int -> String -> Effect Query
-- | updateFoo id newBar =
-- |   build do
-- |     idP <- param id
-- |     newBarP <- param newBar
-- |     pure $
-- |       [ "update foo"
-- |       , "set bar = " <> newBarP
-- |       , "where id = " <> idP
-- |       ]
-- | ```
-- | `updateFoo 1 "test"` will yield:
-- | ```purescript
-- | { text: "update foo\nset bar = $2\nwhere id = $1"
-- | , values: ["test", 1]
-- | }
-- | ```
type QueryBuilderT m a = StateT Query m a
type QueryBuilder a = QueryBuilderT Effect a

-- | Yields a SQL string referencing the last parameter in the parameter list
-- |
-- | Examples:
-- | - if no parameters have been appended this will yield `"$0"` (invalid)
-- | - if 1 parameter has been appended this will yield `"$1"`
-- | - if 5 parameters have been appended this will yield `"$5"`
lastParamString :: forall m. Monad m => QueryBuilderT m String
lastParamString = map (("$" <> _) <<< show <<< Array.length <<< _.values <<< unwrap) $ get

-- | Append a serializable SQL value to the parameter list
appendParam :: forall m a. MonadEffect m => Rep a => a -> QueryBuilderT m Unit
appendParam a =
  let
    values = Proxy @"values"
  in
    do
      a' <- liftEffect $ smash $ serialize a
      void $ modify (Newtype.modify $ Record.modify values (_ <> [ a' ]))

-- | Replace the builder's query string with a new value
putText :: forall m. Monad m => String -> QueryBuilderT m Unit
putText t =
  let
    text = Proxy @"text"
  in
    void $ modify $ Newtype.modify $ Record.set text t

-- | Adds a parameter to the query
-- |
-- | This accepts any value `Rep`resentable in SQL, and
-- | yields the SQL string for the new parameter.
-- |
-- | ```purescript
-- | do
-- |   p1 <- param 1     -- "$1"
-- |   p2 <- param "foo" -- "$2"
-- |   p3 <- param true  -- "$3"
-- |   pure unit
-- | ```
param :: forall m a. MonadEffect m => Rep a => a -> QueryBuilderT m String
param a = do
  appendParam a
  lastParamString

-- | Accepts a `QueryBuilder` monad that yields the built query string
-- | and yields the finished `Query`.
-- |
-- | ```
-- | build $ pure "select 1"
-- | -- Query {text: "select 1", values: [], name: Nothing}
-- | ```
-- | &nbsp;
-- | ```
-- | build do
-- |   foo <- param "foo"
-- |   pure $ "select " <> foo
-- | -- Query {text: "select $1", values: ["foo"], name: Nothing}
-- | ```
build :: QueryBuilder String -> Effect Query
build m = map snd $ build' $ putText =<< m

-- | Executes a `QueryBuilderT`
build' :: forall m a. MonadEffect m => QueryBuilderT m a -> m (a /\ Query)
build' = flip runStateT emptyQuery
