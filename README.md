# postgresql

Purescript PostgreSQL driver

## Table of Contents
 - [Getting Started](#getting-started)
 - [Data](#data)
   - [Ranges](#data---ranges)
 - [Queries](#queries)
   - [Builder](#queries---builder)
 - [Errors](#errors)
 - [Monads](#monads)
   - [`PostgresT`](#monads---postgrest)
   - [`SessionT`](#monads---sessiont)
   - [`CursorT`](#monads---cursort)
 - [`node-postgres` style](#node-postgres-style)

## Getting Started
Install with:
```bash
> spago install postgresql
# (npm | yarn | bun) install pg
```

Next, create a pool [`Config`] object:

```purescript
-- from a connection string:
pgConfig =
  { connectionString: "postgresql://postgres:password@localhost:5432/postgres"
  }

-- or explicitly:
pgConfig =
  { username: "postgres"
  , password: "password"
  , host: "localhost"
  , port: 5432
  , database: "postgres"
  }
```

Then in an `Aff`, use `runPostgres` to connect to the database and execute
queries:
```purescript
module Main where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (launchAff_)
import Effect.Console (log)
import Control.Monad.Postgres (runPostgres, query)
import Effect.Postgres.Error.Except as PG.X

main :: Effect Unit
main =
  launchAff_ do
    msg <- PG.X.run $ runPostgres pgConfig $ query "select 'hello, world!'"
    liftEffect $ log msg -- logs 'hello, world!'
```

[`runPostgres`] creates a connection pool, then executes a
[`PostgresT`](#monads-postgrest) monad, which is an `Aff` with access
to a connection pool.

```purescript
runPostgres :: forall a. <partial config> -> PostgresT Aff a -> Aff a
```

[`query`] accepts any `q` that can be turned [into a query][`AsQuery`]
(here just a `String`), and [unmarshals][`FromRows`] the result into a
destination type `r`.

`query` is from [`MonadSession`], which [`PostgresT`] implements:
```purescript
class MonadSession m where
  query :: forall q r. AsQuery q => FromRows r => q -> m r
  -- ...
```

## Data
Scalar values are serialized & deserialized using the `Serialize` / `Deserialize` typeclasses.
Implemented for `Int`, `String`, `DateTime`, `Buffer`, `BigInt`, `Boolean`, `Number`, `Array`, [`Range`]s, `Maybe`, [`Null`] and `Unit`.

There aren't any typeclass instances for unmarshalling rows by design. The rationale: as apps grow we often need to ask more of a relational database than just CRUD operations. Queries tend to be somewhat living, with joins and columns being added and removed, so it should be easy to modify queries and reflect the change in the purescript type they're unmarshalled into.

The lib does this by transforming the js row array `Array<Array<unknown>>` with `FromRows` ("how many rows do you expect?"), then each row with `FromRow`, then each value with `Deserialize`.

Starting with querying directly into the loose rows as `Array (Array Raw)`:
```purescript
a :: Array (Array Raw) <- query "select a, b, c from (values (1, 'foo', true), (4, 'bar', false)) as foo(a, b, c)"
liftEffect $ log $ show a -- [[1, "foo", true], [4, "bar", false]]
```

We can tell the query to deserialize the rows as `Int /\ String /\ Boolean` (postgres shape of `(int, text, boolean)` ):
```purescript
a :: Array (Int /\ String /\ Boolean) <- query "select a, b, c from (values (1, 'foo', true), (4, 'bar', false)) as foo(a, b, c)"
liftEffect $ log $ show a -- [Tuple 1 (Tuple "foo" true), Tuple 1 (Tuple "foo" true)]
```

From there we could unmarshal to `Maybe` to get 0 or 1, or directly into the row type if expect at least 1 row:
```purescript
a :: Maybe (Int /\ String /\ Boolean) <- query "select a, b, c from (values (1, 'foo', true), (4, 'bar', false)) as foo(a, b, c)"
liftEffect $ log $ show a -- Just (Tuple 1 (Tuple "foo" true))

b :: Int /\ String /\ Boolean <- query "select 1, 'foo', true"
liftEffect $ log $ show b -- Tuple 1 (Tuple "foo" true)

c :: Maybe (Int /\ String /\ Boolean) <- query "select null, null, null limit 0"
liftEffect $ log $ show c -- Nothing
```

`FromRows row` supports `Array row`, `Maybe row` or just `row` (failing if 0 returned).

`FromRow row` supports `Array a`, `Tuple a b`, `Maybe a`, or `a` (where `a` / `b` are `Deserialize`)

The idea is that you can deserialize query results directly to the purescript type you care about:
-   `a :: Int <- query "select 1"`
    - because there's no outer `Array`, we're saying this just returns 1 row. because there's no inner `Array`, we're saying the row just has 1 value; the int!
-   `a :: Array Int <- query "select foo.a from (values (1), (2), (3)) as foo(a)"`
    - Now there's an outer `Array`, so we expect the query to yield multiple rows of shape `(int)`
-   `a :: Array (Maybe Int) <- query "select foo.a from (values (1), (null), (3)) as foo(a)"`
    - Some of them are `NULL`!
- `a :: Int /\ Int <- query "select 1, 2"`
    - 1 row of `(int, int)`
- `a :: Array (Int /\ String) <- query "select id, email from users"`
    - Multiple rows of `(int, string)`
- `a :: Maybe (String /\ String /\ String) <- query $ "select first_name, last_name, email from users where id = $1" /\ userId`
    - 0 or 1 rows of `(text, text, text)`

### Data - Ranges
Postgres ranges are represented with [`Range`].

[`Range`]s can be created with:
 - `mempty` - an unbounded range
 - [`Range.lt`]` a` - `(,a)`
 - [`Range.lte`]` a` - `(,a]`
 - [`Range.gt`]` a` - `(a,)`
 - [`Range.gte`]` a` - `[a,)`

and combined with `append`:
```purescript
mempty <> lt 100 -- (,100)
gte 10 <> lt 100 -- [10,100)
```

## Queries
Queries can be executed with any type that implements [`AsQuery`],
which converts it into a [`Query`]:

```purescript
newtype Query = Query { text :: String, values :: Array Raw, name :: Maybe String }

class AsQuery a where
  asQuery :: a -> Effect Query
```

[`AsQuery`] is implemented for:
 - [`Query`]
 - `String`
 - `String /\ n` where `n` is:
    - n-tuple of [`Rep`] query parameters
    - a single [`Rep`] query parameter
    - `Array `[`Raw`]

### Queries - Builder
For complex parameterized queries, there is a provided [`Query.Builder`]:

```purescript
runPostgres {} do
  exec_ "create table person (id int, first_name text, last_name text, age int, born timestamptz);"
  exec_
    $ Query.Builder.build
    $ do
        id <- Query.Builder.param 1
        firstName <- Query.Builder.param "Henry"
        lastName <- Query.Builder.param "Cavill"
        age <- Query.Builder.param 38
        born <- Query.Builder.param "1985-05-05"
        pure
          $ intercalate "\n"
            [ "insert into person (id, first_name, last_name, age, born)"
            , "values"
            , "("
            , intercalate ", " [id, firstName, lastName, age, born])
            , ")"
            ]
```

[`Query.Builder.param`] accepts any [`Rep`] value and returns a string (ex. `"$2"`)
that will reference that value in the query.

[`Query.Builder.build`] renders the query to [`Query`]

## Errors
Errors are wrapped in [`Error`]:
```haskell
data Error
  = Deserializing Query Foreign.MultipleErrors
  | Serializing Foreign.MultipleErrors
  | Executing Query Effect.Error
  | Connecting Effect.Error
  | Disconnecting Effect.Error
  | Other Effect.Error
```

Most operations in this library return a [`Except`], which is just
`ExceptT` pinned to `NonEmptyArray Error`.

This can be unlifted to `Aff` with [`Except.run`].

## Monads
### Monads - `PostgresT`
[`PostgresT`] is a monadic context with a connection [`Pool`]. It allows running [`transaction`]s, simple [queries][`query`] and more.

#### Running in Aff
```purescript
main :: Effect Unit
main =
  launchAff_ do
    hi <- PG.X.run $ runPostgres {} $ query "select 'hi!'"
    liftEffect $ log hi
```

#### Executing a transaction
```purescript
dbMain :: PostgresT Aff Unit
dbMain = do
  transaction do
    exec_ """
      create table persons
        ( id int primary key generated always as identity
        , name text not null unique
        );
    """
    exec_ $ "insert into persons (name) values ($1);" /\ "Henry"
  pure unit
```

#### Simple queries
```purescript
dbMain :: PostgresT Aff Int
dbMain = exec_ $ "insert into persons (name) values ($1);" /\ "Sarah"
-- equivalent to:
-- dbMain = session $ exec_ ...
```

#### Executing cursor queries
```purescript
dbMain :: PostgresT Aff Int
dbMain =
  cursor @(Int /\ String) "people_cursor" "select id, name from persons" do
    a <- fetchOne -- Just (1 /\ "Henry")
    b <- fetchOne -- Just (2 /\ "Sarah")
    void $ move (MoveRelative -2)
    c <- fetchAll -- [1 /\ "Henry", 2 /\ "Sarah"]
    d <- fetchOne -- Nothing
```

### Monads - `SessionT`
[`SessionT`] is a context that can run queries using a specific [`Client`].

Sessions can be started in [`PostgresT`], which will ask the [`Pool`] for a [`Client`],
then release the [`Client`] after the session completes.

Within sessions, queries can be performed with [`query`], [`exec`] and [`exec_`].

### Monads - `CursorT`
[`CursorT`] is a transaction [`SessionT`] with access to a named server-side cursor.

Run in [`PostgresT`] with [`cursor`]

## `node-postgres` style
You may also choose to use the `Aff` API directly, which closely mirrors
the api of [`node-postgres`]:

- [`Client`]
  - create with [`Client.make`] or [`Client.connected`]
  - execute queries with [`Client.query`], [`Client.queryRaw`] or [`Client.exec`]
  - release with [`Client.end`]
- [`Pool`]
  - create with [`Pool.make`]
  - issue clients with [`Pool.connect`]
  - release clients with [`Pool.release`] or [`Pool.destroy`]
  - release with [`Pool.end`]

[`Pool`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Effect.Aff.Postgres.Pool#t:Pool
[`Config`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Effect.Aff.Postgres.Pool#t:Config
[`Pool.make`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Effect.Aff.Postgres.Pool#v:make
[`Pool.end`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Effect.Aff.Postgres.Pool#v:end
[`Pool.connect`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Effect.Aff.Postgres.Pool#v:connect
[`Pool.destroy`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Effect.Aff.Postgres.Pool#v:destroy
[`Pool.release`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Effect.Aff.Postgres.Pool#v:release

[`Client`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Effect.Aff.Postgres.Client#t:Client
[`Client.end`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Effect.Aff.Postgres.Client#v:end
[`Client.make`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Effect.Aff.Postgres.Client#v:make
[`Client.connected`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Effect.Aff.Postgres.Client#v:connected
[`Client.query`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Effect.Aff.Postgres.Client#v:query
[`Client.queryRaw`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Effect.Aff.Postgres.Client#v:queryRaw
[`Client.exec`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Effect.Aff.Postgres.Client#v:exec

[`Range`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Data.Postgres.Range#t:Range
[`Range.gt`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Data.Postgres.Range#v:gt
[`Range.gte`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Data.Postgres.Range#v:gte
[`Range.lt`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Data.Postgres.Range#v:lt
[`Range.lte`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Data.Postgres.Range#v:lte

[`Raw`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Data.Postgres.Raw#t:Raw
[`Null`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Data.Postgres.Raw#t:Null

[`Serialize`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Data.Postgres#t:Serialize
[`Deserialize`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Data.Postgres#t:Deserialize
[`Rep`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Data.Postgres#t:Rep
[`modifyPgTypes`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Data.Postgres#v:modifyPgTypes

[`Result`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Data.Postgres.Result#t:Result
[`FromRow`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Data.Postgres.Result#t:FromRow
[`FromRows`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Data.Postgres.Result#t:FromRows

[`Query`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Data.Postgres.Query#t:Query
[`AsQuery`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Data.Postgres.Query#t:AsQuery

[`Query.Builder`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Data.Postgres.Query.Builder#t:Builder
[`Query.Builder.param`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Data.Postgres.Query.Builder#v:param
[`Query.Builder.build`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Data.Postgres.Query.Builder#v:build

[`Except`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Effect.Postgres.Error.Except#t:Except
[`Error`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Effect.Postgres.Error#t:Error
[`Except.run`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Effect.Postgres.Error.Except#v:run

[`MonadCursor`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Control.Monad.Postgres#t:MonadCursor
[`MonadSession`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Control.Monad.Postgres#t:MonadSession
[`CursorT`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Control.Monad.Postgres#t:CursorT
[`SessionT`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Control.Monad.Postgres#t:SessionT
[`PostgresT`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Control.Monad.Postgres#t:PostgresT
[`cursor`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Control.Monad.Postgres#v:cursor
[`session`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Control.Monad.Postgres#v:session
[`transaction`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Control.Monad.Postgres#v:transaction
[`runPostgres`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Control.Monad.Postgres#v:runPostgres
[`query`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Control.Monad.Postgres#v:query
[`exec`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Control.Monad.Postgres#v:exec
[`exec_`]: https://pursuit.purescript.org/packages/purescript-postgresql/2.0.21/docs/Control.Monad.Postgres#v:exec_

[`node-postgres`]: https://node-postgres.com/
[`pg-types`]: https://github.com/brianc/node-pg-types/
