**Table of contents**

- [Introduction](#introduction)
- [Setting up a table in PostgreSQL for our users](#setting-up-a-table-in-postgresql-for-our-users)
- [The `User` module](#the-user-module)
    - [Instances for JSON encoding/decoding and extraction from the database](#instances-for-json-encodingdecoding-and-extraction-from-the-database)
    - [Adding, deleting and listing users in our PostgreSQL table from Haskell](#adding-deleting-and-listing-users-in-our-postgresql-table-from-haskell)
    - [Indexing `User`s in terms scotty understands](#indexing-users-in-terms-scotty-understands)
- [Error handling](#error-handling)
    - [The `ServiceError` type](#the-serviceerror-type)
    - [Functions for catching constraint violations and SQL errors](#functions-for-catching-constraint-violations-and-sql-errors)
- [Main module: the webservice](#main-module-the-webservice)
    - [Connecting to PostgreSQL](#connecting-to-postgresql)
    - [The *users* resource](#the-users-resource)
    - [Get the party started](#get-the-party-started)
    - [Calling for a service](#calling-for-a-service)

# Introduction

If you've seen [the README](https://github.com/zalora/servant/blob/master/README.md) before landing here, you've probably seen:

> **servant** is at its core a type-safe and *very extensible* library for defining REST-y webservices around some core "database operations", in just a few very simple lines, e.g:
>
> ``` haskell
> mkResource "users" pgsqlcontext pgexceptions
>   & addWith Users.add
>   & listAllWith Users.view
>   & deleteWith Users.delete
> ```
>
> which can then be turned into a [scotty](http://hackage.haskell.org/package/scotty) webservice by calling `runResource` (from *servant-scotty* -- the only backend we have for now) on it.

Now this isn't the most useful sample of code, so the goal in this document is just to write a small webservice around some `User` data type, that'll let us add, delete and list `User`s. Note that the example program we'll write is available in this repository, in the [`servant-example/`](https://github.com/zalora/servant/tree/master/servant-example) directory, as a cabalized and runnable project along with some SQL code to run to setup a postgresql table.

# Setting up a table in PostgreSQL for our users

Let's create a table to hold our dummy data. Just put the following SQL code into a file, say `setup.sql`.

``` sql
CREATE TABLE IF NOT EXISTS users
  ( email text primary key
  , karma int default 0
  );
```

And then gently ask postgresql to run it:

``` bash
$ psql -f setup.sql
```

Alright, you're good to go!

# The `User` module

We'll be dealing with users, which are composed of an email address (by which users are indexed) and some karma (an `int`).

So let's first represent that in Haskell. Let's create a `Model/User.hs` module. First, language extensions & imports boilerplate.

``` haskell
{-# LANGUAGE DeriveGeneric, TypeSynonymInstances, OverloadedStrings #-}
module Model.User where

import Control.Applicative
import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Servant.PostgreSQL.Prelude
import Servant.Scotty.Prelude
```

Now, let's define the Haskell data type corresponding to one row in our `users` table.

``` haskell
type Email = Text

data User =
  User { email :: Email
       , karma :: Int
       } deriving (Eq, Show, Generic)
```

## Instances for JSON encoding/decoding and extraction from the database
We derive the `Generic` class so that we don't even have to write `FromJSON/ToJSON` instances, they can be derived automatically:

``` haskell
instance ToJSON User where
instance FromJSON User where
```

While we're at writing simple instances, we'll also need one to extract a `User` from our `users` table, which means we must write a `FromRow` instance for `User`. There you go:

``` haskell
instance FromRow User where
  fromRow = User <$> field <*> field
```

This just tries to extract a `Text` field and an `Int` field, in that order,sticking them together in a `User` value if it succeeds.

## Adding, deleting and listing users in our PostgreSQL table from Haskell

This is quite straighforward. Notice that the listing query takes advantage of the `FromRow` instance we've just defined.

``` haskell
addUser :: User -> Connection -> IO Int64
addUser user conn =
  execute conn "insert into users(email, karma) values (?, ?)"
               (email user, karma user)

deleteUser :: Email -> Connection -> IO Int64
deleteUser email conn =
  execute conn "delete from users where email = ?" (Only email)

listUsers :: Connection -> IO [User]
listUsers conn =
  query_ conn "select email, karma from users"
```

Except that... We'll need to make a slight tweak, fortunately an easy one to motivate.

So here is the thing: *servant* has some machinery to automatically turn database operation results into a proper response to send, only in JSON in the standard operations provided by *servant* for now. But it does that using... guess what? **Types** and **typeclasses** of course! If we're adding an user and it succeeds, we want HTTP status code 201 to be sent, but when deleting an user successfully, we want status code 200. And if there's an error when adding a user, we want status code 400, but on the other hand if we ask to delete an user with an unexisting email, we want status code 404 to be send to the client.

This necessary flexibility means we have to go through some wrapper around `Int64` that's tagged by something specific to the operation. *servant* provides both a useful wrapper around `Int64` tagged with some phantom type *and* standard operations through these (empty) types: `Add`, `Delete`, `ListAll`, `Update`, `View`.

From the *servant-postgresql* package:

``` haskell
-- | A wrapper around 'Int64', which is what
--   PG hands us back when running
--   'Database.PostgreSQL.Simple.execute'.
--
--   The @o@ type parameter lets us tag
--   the result with the operation that
--   we're running. This lets us turn
--   results into a proper response
--   (response body + status) differently
--   for 'Add' and 'Update' for example.
newtype PGResult o = PGResult { pgres :: Int64 }
```

And we have:

``` haskell
-- | Run an 'IO' action that returns 'Int64' and
--   convert the result to a 'PGResult'.
pgresultOfInt64 :: IO Int64 -> IO (PGResult o)
```

So we'll use that `pgresultOfInt64` function for `addUser` and `deleteUser` and tag `PGResult` with `Add` and `Delete` respectively. The final code for our postgresql queries is:

``` haskell
-- notice the new return type
addUser :: User -> Connection -> IO (PGResult Add)
addUser user conn = pgresultOfInt64 $ -- conversion
  execute conn "insert into users(email, karma) values (?, ?)"
               (email user, karma user)

-- notice the new return type
deleteUser :: Email -> Connection -> IO (PGResult Delete)
deleteUser email conn = pgresultOfInt64 $ -- conversion
  execute conn "delete from users where email = ?" (Only email)

listUsers :: Connection -> IO [User]
listUsers conn =
  query_ conn "select email, karma from users"
```

## Indexing `User`s in terms scotty understands

One last thing that has to do with our `User` type. Now we get to the *servant*, and in particular *servant-scotty* specific bits. We'll specify how to fetch our *index* type, `Email` (which is just a synonym for `Text`), from URLs, as well as the bit of the route pattern one should insert to reference a particular `User` entry. You'll quickly understand.

For cleaner code, you'd generally use a newtype to prevent the following instance from being a `TypeSynonymInstance`.

``` haskell
instance Index Email where
  -- To fetch the email that references an User
  -- we just look up the value of the "email" parameter
  -- in the request path.
  --
  -- E.g, if we issue the following request:
  --   DELETE /users/alp@blah.com
  -- and given this route pattern:
  --   /users/:email
  -- we want to lookup "alp@blah.com"
  idx = param "email"

  -- A "part of a route pattern" we can just reuse whenever we
  -- need to generate and endpoint that requires looking up
  -- parameters in the request path.
  route _ = "/:email"
```

... And we're done with this module! Phew.

# Error handling

Before we get to enjoy our succinct, simple and minimalist webservice resource declaration, we need write some code that'll let us run our postgresql queries in an exception-safe way, relying on a default error handler in your scotty web app for taking care of sending an appropriate response to the client.

We'll create an `Error.hs` module to store the error type of our scotty app and some functions that convert `postgresql-simple` exceptions to our error type.

First off, some boilerplate.

``` haskell
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Error where

import Data.Aeson
import Data.Monoid
import Data.Text (Text, pack)
import Data.Text.Lazy (fromStrict)
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Errors
import GHC.Generics
import Servant.Error
import Web.Scotty.Trans
```

## The `ServiceError` type

No fancy needs in our case, let's just make it a wrapper around `Text` and derive JSON encoding/decoding instances automatically.

``` haskell
newtype ServiceError =
    ServiceError { msg :: Text }
    deriving (Eq, Show, Generic)

instance FromJSON ServiceError where
instance ToJSON ServiceError where
```

We'll also need to make our type an instance of `ScottyError` so that it integrates well with scotty's error handling. This is however *really* straighforward.

``` haskell
instance ScottyError ServiceError where
  stringError s = ServiceError (pack s)

  showError (ServiceError err) = fromStrict err
```

## Functions for catching constraint violations and SQL errors

These are straightforward too: we're basically extracting bits of information from the error types of [postgresql-simple](http://hackage.haskell.org/package/postgresql-simple) and glueing them into some intelligible error message.

The only thing is that there are some new types. An `ExceptionCatcher ServiceError` value is just some function of type `except -> ServiceError` for some precise type `except` that's an instance of `Exception`. The wrapping is done by calling `catchAnd`. Since you supply it with a conversion function, this is meant to be read as *catch-and-convert-<the-exception>*.

``` haskell
sqlerrorCatcher :: ExceptionCatcher ServiceError
sqlerrorCatcher = catchAnd convertError
  where convertError sqlerr =
          ServiceError $
            "sql error: " <> decodeUtf8 (sqlErrorMsg sqlerr)

violationsCatcher :: ExceptionCatcher ServiceError
violationsCatcher = catchAnd (ServiceError . cvToText)
  where
    cvToText :: ConstraintViolation -> Text
    cvToText (NotNullViolation field) =
      "field '" <> decodeUtf8 field <> "' shouldn't be NULL"

    cvToText (ForeignKeyViolation table constraint) =
      "foreign key constraint '" <> decodeUtf8 constraint <>
      "' on table '" <> decodeUtf8 table <> "' violated"

    cvToText (UniqueViolation constraint) =
      "unique constraint '" <> decodeUtf8 constraint <> "' violated"

    cvToText (CheckViolation table constraint) =
      "check of constraint '" <> decodeUtf8 constraint <>
      "' on table '" <> decodeUtf8 table <> "' violated"
```

And we're done with error handling! You'll see in the next section how we then ask servant to watch for these exceptions using these conversion functions when one is triggered.

# Main module: the webservice

It's now time to actually generate a webservice from these things. The whole goal of *servant* has been to make webservice declaration succinct, non-repetitive and to be able to reuse operations on different types, because whether we're adding users or books to our database doesn't change much except the actual SQL query, everything else that's going on is really just the same for both.

So let's start, as usual, with the module header boilerplate for our `Main.hs` module.

``` haskell
{-# LANGUAGE DataKinds, OverloadedStrings #-}
module Main where

import Data.Int
import Data.Monoid
import Database.PostgreSQL.Simple (Connection)
import Servant.PostgreSQL.Prelude
import Servant.Scotty
import Servant.Scotty.Prelude
import Web.Scotty.Trans

import Error
import Model.User
```

## Connecting to PostgreSQL

The first thing we need to take care of is to tell *servant* how it should get its hands on a PostgreSQL connection, to send queries through that connection. In *servant* terminology, these connection-ish things are called **contexts**. You don't need to know much about them for now since *servant* provides helper functions to create these `Context`s. In particular, with just one small function call, we can tell servant it should use a pool of PostgreSQL connections whenever it'll want to call one of our three queries from the `Model.User` module. Here it is:

``` haskell
-- 'Connection' refers to postgresql-simple's 'Connection' type
getContext :: IO (Context Connection)
getContext = 
  pooledContextOfConnStr 10 -- number of "sub-pool"
                         10 -- keep idle connections during 10 secs
                         100 -- max. 100 simultaneous open connections per sub-pool
                         connstring -- postgresql connection string
  where connstring = "host=localhost user=alp"
        -- in a real webservice, this would obviously be
        -- fetched from a config file or from the
        -- command-line arguments
```

This is all *servant* needs to know in order to connect to your postgresql!

## The *users* resource

Alright, let's now declare a *servant* `Resource` to manage `User`s that supports adding, deleting and listing users, using our functions from `Model.User`. It'll catch the exceptions we've taken care of in our `Error` module and use a postgresql context it takes as an argument, which will be the one we've just defined.

``` haskell
users ctx =
  mkResource "users" ctx exceptions
    & addWith     addUser
    & deleteWith  deleteUser
    & listAllWith listUsers

  where exceptions = sqlerrorCatcher <> violationsCatcher
        -- we just mappend the two catchers together
        -- to create one that'll watch for both
        -- exception types and call the appropriate
        -- conversion function to turn the said exception
        -- into our ServiceError type
```

Well, this is surprisingly simple! And we can turn this into scotty endpoints in just one function call. But just before we do that, let's take a look at the type of this function.

``` haskell
users :: Context Connection
      -> Resource Connection
                  User
                  Email
                  PGResult
                  ServiceError
                  '[ListAll, Delete, Add]
```

Mmm! This `Resource` type surely is parametrized by many others! Let's breakdown what each type represents.

- The `Context Connection` argument will just be what we get from `getContext` above.
- *servant*'s `Resource` type is parametrized by several things. The general type is:
    
    ``` haskell
    data Resource c a i (r :: * -> *) e (ops :: [*]) = ...
    ```

    where:

    - `c` is the type of the connection-ish thing we use. In our case: a PostgreSQL `Connection`.
    - `a` is the type of the entries we manage. In our case: `User`s.
    - `i` is the type used to index the entries. In our case: `Email`.
    - `r` is the type returned by our "effectful" database operations: the ones that add, delete, update entries, for example. It has kind `* -> *` because we want to tag it with the dummy type that designates an operation. In our case, that's just `PGResult` from *servant-postgresql*.
    - `e` is the error type to which we convert any exception type we're watching for if one is triggered. In our case: `ServiceError`.
    - `ops` is a [type-level list](https://www.haskell.org/ghc/docs/latest/html/users_guide/promotion.html#promoted-lists-and-tuples) of the dummy types designating the operations we provide support for in our `Resource`. In our case: `'[ListAll, Delete, Add]`.
    
    Note that `ops` would be different if we just had:

    ``` haskell
    users ctx =
      mkResource "users" ctx exceptions
        & addWith     addUser
        & listAllWith listUsers
    
      where exceptions = sqlerrorCatcher <> violationsCatcher
    ```

    It would have been just `'[ListAll, Add]` in that case. 

    Carrying this information at the type-level and maintaining a list internally with `addUser` and `listUsers` lets us have a strong, composable and type-safe way to generate endpoints for the resource, or for example to generate some documentation for the webservice's API (I have some ideas for this). Always the same principle!

## Get the party started

We now have everything we need to write our `main` function that'll fire up a scotty webservice to manage our users.

``` haskell
main :: IO ()
main = do
  -- we initialize our connection pool context
  ctx <- getContext

  -- we run a web application that listens on
  -- port 4321
  scottyT 4321 id id $ do
    -- a default error handler that sends
    -- status code 400 along with the ServiceError
    -- value as JSON
    defaultHandler $ \err -> do
      status badRequest400
      json err

    -- magic: we turn our Resource description into
    -- three endpoints:

    -- * GET /users will list all users in JSON.
    -- * POST /users will try to decode a user in JSON format
    --   from the request body and add it to the database, returning
    --   some useful response in JSON along with the appropriate status
    --   depending on whether the user was successfully added.
    -- * DELETE /users/alp@blah.com will delete (if it exists) the user
    --   with this email from the database, responding in JSON with some
    --   information on how things went, just like in the previous endpoint.
    runResource $ users ctx
```

## Calling for a service

Now if you build and run this application, you can see it shine through curl:


``` bash
# we begin with no user
$ curl http://localhost:4321/users
[]
# let's add yours truly
$ curl -X POST -d '{ "email": "alp@zalora.com", "karma" : 15 }' http://localhost:4321/users
{"success":true,"msg":""}
# have I really been added? yes!
$ curl http://localhost:4321/users
[{"email":"alp@zalora.com","karma":15}]
# and I let myself go...
$ curl -X DELETE http://localhost:4321/users/alp@zalora.com
{"success":true,"msg":""}
# ... did it work?
$ curl http://localhost:4321/users
[]
# by the way, are we really catching exceptions?
# i'll just try to add myself twice now.
$ curl -X POST -d '{ "email": "alp@zalora.com", "karma" : 15 }' http://localhost:4321/users
{"success":true,"msg":""}
$ curl -X POST -d '{ "email": "alp@zalora.com", "karma" : 15 }' http://localhost:4321/users
{"msg":"sql error: duplicate key value violates unique constraint \"users_pkey\""}
```
