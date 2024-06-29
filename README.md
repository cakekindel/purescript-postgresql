# postgresql

Purescript PostgreSQL driver

## Table of Contents
 - [Getting Started](#getting-started)
 - [Data](#data)
   - [Rows](#data---rows)
   - [Ranges](#data---ranges)
 - [Queries](#queries)
   - [Builder](#queries---builder)
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

main :: Effect Unit
main =
  launchAff_ do
    msg <- runPostgres pgConfig $ query "select 'hello, world!'"
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
Single SQL values are serialized to and deserialized from JS via [`pg-types`]
(with some [tweaks][`modifyPgTypes`]).

The conversion between [`Raw`] JS values and purescript values is done
with the [`Serialize`] and [`Deserialize`] typeclasses.

The [`Rep`] class indicates a type is [`Rep`]resentable as a SQL value.
[`Rep`] is automatically implemented for all types that are [`Serialize`]
and [`Deserialize`].

Implementations are provided for `Int`, `String`, `DateTime`, `Buffer`,
`BigInt`, `Boolean`, `Number`, `Array`, [`Range`]s, `Maybe`, [`Null`]
and `Unit`.

### Data - Rows
A single row (multiple SQL values) are deserialized using [`FromRow`],
which is implemented for:
 - n-tuples of [`Rep`] types
 - `Array a` where `a` is [`Rep`]
 - A single [`Rep`] type

Examples:
```purescript
(fromRow []     :: Maybe Int)  == Nothing
(fromRow [1]    :: Maybe Int)  == Just 1
(fromRow [1, 2] :: Maybe Int)  == Just 1
(fromRow []     :: Int /\ Int) == Error
(fromRow [1, 2] :: Int /\ Int) == 1 /\ 2
(fromRow []     :: Array Int)  == []
(fromRow [1, 2] :: Array Int)  == [1, 2]
```

Multiple rows are deserialized using [`FromRows`],
which is implemented for:
 - `Array a` where `a` is [`FromRow`]
 - `Maybe a` where `a` is [`FromRow`] (equivalent to `Array.head <<< fromRows`)
 - `a` where `a` is [`FromRow`] (throws if 0 rows yielded)
 - `RowsAffected`
     - Extracts the number of rows processed by the last command in the query (ex. `INSERT INTO foo (bar) VALUES ('a'), ('b')` -> `INSERT 2` -> `RowsAffected 2`)

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

## Monads
### Monads - `PostgresT`
[`PostgresT`] is the database driver's main entry point,
and is just an `Aff` with access to a [`Pool`].

Run in `Aff` with [`runPostgres`]:
```purescript
main :: Effect Unit
main =
  launchAff_ do
    hi <- runPostgres {} $ query "select 'hi!'"
    liftEffect $ log hi
```

Execute [`SessionT`] monads with [`session`] or [`transaction`]:
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

Implements [`MonadSession`] as a shorthand for single-query [`session`]s:
```purescript
dbMain :: PostgresT Aff Int
dbMain = exec_ $ "insert into persons (name) values ($1);" /\ "Sarah"
-- equivalent to:
-- dbMain = session $ exec_ ...
```

Execute [`CursorT`] monads with [`cursor`]:
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
[`SessionT`] is an `Aff` with access to a [`Client`]
issued by a [`Pool`], connected to the database.

Run in [`PostgresT`] with [`session`] or [`transaction`]

Perform queries with [`query`], [`exec`] or [`exec_`]

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

[`Pool`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Effect.Aff.Postgres.Pool#t:Pool
[`Config`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Effect.Aff.Postgres.Pool#t:Config
[`Pool.make`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Effect.Aff.Postgres.Pool#v:make
[`Pool.end`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Effect.Aff.Postgres.Pool#v:end
[`Pool.connect`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Effect.Aff.Postgres.Pool#v:connect
[`Pool.destroy`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Effect.Aff.Postgres.Pool#v:destroy
[`Pool.release`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Effect.Aff.Postgres.Pool#v:release

[`Client`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Effect.Aff.Postgres.Client#t:Client
[`Client.end`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Effect.Aff.Postgres.Client#v:end
[`Client.make`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Effect.Aff.Postgres.Client#v:make
[`Client.connected`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Effect.Aff.Postgres.Client#v:connected
[`Client.query`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Effect.Aff.Postgres.Client#v:query
[`Client.queryRaw`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Effect.Aff.Postgres.Client#v:queryRaw
[`Client.exec`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Effect.Aff.Postgres.Client#v:exec

[`Range`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Data.Postgres.Range#t:Range
[`Range.gt`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Data.Postgres.Range#v:gt
[`Range.gte`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Data.Postgres.Range#v:gte
[`Range.lt`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Data.Postgres.Range#v:lt
[`Range.lte`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Data.Postgres.Range#v:lte

[`Raw`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Data.Postgres.Raw#t:Raw
[`Null`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Data.Postgres.Raw#t:Null

[`Serialize`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Data.Postgres#t:Serialize
[`Deserialize`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Data.Postgres#t:Deserialize
[`Rep`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Data.Postgres#t:Rep
[`modifyPgTypes`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Data.Postgres#v:modifyPgTypes

[`Result`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Data.Postgres.Result#t:Result
[`FromRow`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Data.Postgres.Result#t:FromRow
[`FromRows`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Data.Postgres.Result#t:FromRows

[`Query`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Data.Postgres.Query#t:Query
[`AsQuery`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Data.Postgres.Query#t:AsQuery

[`Query.Builder`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Data.Postgres.Query.Builder#t:Builder
[`Query.Builder.param`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Data.Postgres.Query.Builder#v:param
[`Query.Builder.build`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Data.Postgres.Query.Builder#v:build

[`MonadCursor`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Control.Monad.Postgres#t:MonadCursor
[`MonadSession`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Control.Monad.Postgres#t:MonadSession
[`CursorT`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Control.Monad.Postgres#t:CursorT
[`SessionT`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Control.Monad.Postgres#t:SessionT
[`PostgresT`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Control.Monad.Postgres#t:PostgresT
[`cursor`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Control.Monad.Postgres#v:cursor
[`session`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Control.Monad.Postgres#v:session
[`transaction`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Control.Monad.Postgres#v:transaction
[`runPostgres`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Control.Monad.Postgres#v:runPostgres
[`query`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Control.Monad.Postgres#v:query
[`exec`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Control.Monad.Postgres#v:exec
[`exec_`]: https://pursuit.purescript.org///////////////////////////////////////////////////////////packages/purescript-postgresql/2.0.16/Control.Monad.Postgres#v:exec_

[`node-postgres`]: https://node-postgres.com/
[`pg-types`]: https://github.com/brianc/node-pg-types/
