At *Zalora*, we've been writing quite a few webservices and web applications in general. After having myself written a couple of these, I ended up being completely *sick* of this feeling that most of the code just looked really similar. We've been using [scotty](http://hackage.haskell.org/package/scotty), and when we're writing RESTy web services, the code just looks like:

``` haskell
-- endpoints for managing users
get "/users" $ -- use the database to list users in json
post "/users" $ -- use the database to add an user
get "/users/:userid" $ -- use the database to view a particular user

-- where in each endpoint we are careful of fetching data from URLs or
-- request bodies, watching for exceptions around DB operations etc
```

a single webservice being comprised of a bunch of such groups of endpoints.

And we'd just repeat this all over the place. Of course, we quickly abstracted away a couple of common scenarios, but nothing satisfaction-worthy. This led to a small journey toward a better world, where we could just declare a webservice with something like:

``` haskell
runResource $
  mkResource "users" pgsqlcontext exceptioncatchers
    & addWith Users.add
    & listAllWith Users.listAll
    & viewWith Users.view
```

where `runResource` translates what looks like some kind of DSL for describing resources into a concrete webservice. This would let us focus on
these `Users.add`, `Users.listAll` and `Users.view` functions, which here could look like:

``` haskell
add :: User -> Connection -> IO Int64
listAll :: Connection -> IO [User]
view :: UserId -> Connection -> IO (Maybe User)
```

And ideally we even would like to be able define our own "operations", just like adding, viewing and listing all entries above. For example, we may want for our service to have an endpoint to search for users.

Even nicer would be to be able to switch from a web framework to another (or even have a terminal interface for quick tests), by just using a different `runResource`.

And we came up with a solution for this. The rest of this tutorial describes how to use our solution and explains a bit how it works. Why the latter? Because *servant* has been written with extensibility in mind, and there are several ways you can extend it for your custom input/output formats, web framework, operations and what not.

# Hello servant

Alright, let's get started by taking a look at the servant packages and seeing what each one of them brings to the table.

## servant

The `servant` package contains the core types and functions to let you describe a `Resource` just from the actual database operations, like our `add`, `listAll` and `update` functions above, and a couple of other necessary details.

### The `Resource` type

One of the things it defines is the `Resource` type, which carries all the necessary information to fully describe a `Resource` so that it can later be "interpreted" in some web framework or in another way, e.g for generating docs (I've already planned to work on this). From the code itself:

``` haskell
-- | A resource that:
--
--   * uses some context type @c@ (think database connection)
--   * manages entries of type @a@
--   * (optionally) supports indexing through the @i@ type (a dumb ID, or something like
--     @data FooId = ByToken Token | ByID Integer@). That can be useful when trying to view,
--     update or delete a particular entry, for example.
--   * uses @r@ as the return type (tagged by the operation type) for /effectful/ database operations (e.g. adding, updating, deleting entries
--     for example).
--   * can catch exceptions, converting them to some error type
--     @e@ of yours
--   * supports the operations listed in the @ops@ type list. a corresponding
--     (heterogeneous) list of "database functions" is held internally and we
--     ask the compiler to make the types of these functions match with the ones
--     expected for the operations listed at the type-level.
data Resource c a i (r :: * -> *) e (ops :: [*])
  = Resource { name       :: String    -- ^ Name of the 'Resource'
             , context    :: Context c -- ^ 'Context' attached to this 'Resource'
             , excCatcher :: ExceptionCatcher e -- ^ Hands you the 'ExceptionCatcher' you can call
                                                --   'handledWith' with to make your \"database operations\"
                                                --   exception safe.
             , operations :: HList (Ops ops c a i r)
             }
```

Aside from the unusual number of type variables (which however *is* necessary here, we get used to it quite quickly don't worry), the only "tricky" thing here is that `operations` heterogeneous list field.

- The `name` field (accessor) is useful when generating endpoints, is used in the `Show` instance for `Resource` and can be useful for anything you like.
- The `context` wraps a function that lets you operate on some "database connection"-ish thing of type `c`, i.e something with type `forall r. (c -> IO r) -> IO r`. Think of it as a `withConnection` function if you want. See `Servant.Context` in the *servant* package to see how one should be used and how to create your own contexts. The *servant-pool* and *servant-postgresql* packages provide some helper functions for creating `Context`s that respectively use [pooling](http://hackage.haskell.org/package/resource-pool) and [postgresql](http://hackage.haskell.org/package/postgresql-simple).
- `excCatcher` is basically a list of functions, where each function has type `except -> e` for some `except` type that's an instance of the `Exception` class from `Control.Exception`. The `Servant.Error` module contains functions to create and combine catchers together. The idea is that we can catch (if one arises) exceptions for every `except` type represented in this list and then convert the exception value to our `Resource`'s error type `e`. You can then use that value in your web-service to report errors appropriately using a customer `defaultHandler` in scotty for example.
- `operations` is a list of functions that have different types. Every time we add support for some operation on our `Resource`, using functions like `addWith`, `listAllWith` or `updateWith` above, we add the specified function in this list (`Users.add`, `Users.listAll`, `Users.update` in our example above) and the corresponding operation type (`Add`, `ListAll`, `Update` in our example), which is really only meant to be used at the type-level and shouldn't actually contain anything, gets added to the type-level list `ops`. The type of the `operations` field may look a bit scary but this is just for enforcing that the list of functions matches the expected types for the corresponding operations.
