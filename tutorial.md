**Table of contents**

- [Introduction](#introduction)
- [The servant packages](#the-servant-packages)
    - [servant](#servant)
        - [The `Resource` type](#the-resource-type)
        - [Creating "an empty `Resource`"](#creating-an-empty-resource)
        - [Operations](#operations)
        - [Abstract... ?](#abstract-)
    - [servant-scotty](#servantscotty)
        - [Running a `Resource`](#running-a-resource)
        - [Running an operation: `ScottyOp`](#running-an-operation-scottyop)
            - [Example: `ListAll`](#example-listall)
            - [Example: `View`](#example-view)
    - [servant-response](#servantresponse)
    - [servant-postgresql](#servantpostgresql)
        - [Creating a PostgreSQL `Context`](#creating-a-postgresql-context)
        - [Generating responses](#generating-responses)
    - [servant-pool](#servantpool)

# Introduction

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
add :: User -> Connection -> IO (PGResult Add)
listAll :: Connection -> IO [User]
view :: UserId -> Connection -> IO (Maybe User)
```

And ideally we even would like to be able define our own "operations", just like adding, viewing and listing all entries above. For example, we may want for our service to have an endpoint to search for users.

Even nicer would be to be able to switch from a web framework to another (or even have a terminal interface for quick tests), by just using a different `runResource`.

And we came up with a solution for this. The rest of this tutorial describes how to use our solution and explains a bit how it works. Why the latter? Because *servant* has been written with extensibility in mind, and there are several ways you can extend it for your custom input/output formats, web framework, operations and what not.

# The servant packages

Alright, let's take a look at the servant packages and see what each one of them brings to the table.

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
- The `context` wraps a function that lets you operate on some "database connection"-ish thing of type `c`, i.e something with type `forall r. (c -> IO r) -> IO r`. Think of it as a `withConnection` function where we've already specified how we get our hand on the `Connection`, if you want. See `Servant.Context` in the *servant* package to see how one should be used and how to create your own contexts. The *servant-pool* and *servant-postgresql* packages provide some helper functions for creating `Context`s that respectively use [pooling](http://hackage.haskell.org/package/resource-pool) and [postgresql](http://hackage.haskell.org/package/postgresql-simple).
- `excCatcher` is basically a list of functions, where each function has type `except -> e` for some `except` type that's an instance of the `Exception` class from `Control.Exception`. The `Servant.Error` module contains functions to create and combine catchers together. The idea is that we can catch exceptions (if one arises) for every `except` type represented in this list and then convert the exception value to our `Resource`'s error type `e`. You can then use that value in your web-service to report errors appropriately using a custom `defaultHandler` in scotty for example.
- `operations` is a list of functions that have different types. Every time we add support for some operation on our `Resource`, using functions like `addWith`, `listAllWith` or `updateWith` above, we add the specified function in this list (`Users.add`, `Users.listAll`, `Users.update` in our example above) and the corresponding operation type (`Add`, `ListAll`, `Update` in our example), which is really only meant to be used at the type-level and shouldn't actually contain anything, gets added to the type-level list `ops`. The type of the `operations` field may look a bit scary but this is just for enforcing that the list of functions matches the expected types for the corresponding operations.

### Creating "an empty `Resource`"

There's only one way to create a `Resource` (to which you can add support for some operations later on): `mkResource`.

``` haskell
mkResource :: String
           -> Context c
           -> ExceptionCatcher e
           -> Resource c a i r e '[]
```

It creates a resource that supports no operation at all. Just give it a name, some context and a list of exceptions to watch for (or none at all), and it'll be ready to receive support for operations later on.

### Operations

I've been talking about operations so far without saying much about them, except that at its core an operation is juste a type, which usually is empty. The standard operations provided by *servant* (in `Servant.Prelude`) actually **are** empty types:

``` haskell
data Add
data Delete
data ListAll
data Update
data View
```

These are only going to be used in servant's type-level manipulations and lets one define some associated information to an operation. The first instance of "associated information" you'll encounter is:

``` haskell
type family Operation o c a i (r :: * -> *) :: *
```

An important thing to understand in servant is that for a given operation (let's say `Add`), you are forced to provide a "database operation" of a precise type, which can be a mix of the context type, the type of entries managed by the resource, the type by which our entries are indexed, the return type of effectful database operations (think add, update, delete) `r` tagged by the operation type, or any concrete type you want (a timestamp argument, some text, really anything you want). For `Add` we have this instance:

``` haskell
type instance Operation Add c a i r = a -> c -> IO (r Add)
```

Which means if you want to use `addWith` for your precise type of entries, you have to provide `addWith` with a function that looks like this, replacing `a` by the type of your entries, `c` by your "connection type", and `r` by `PGResult` from *servant-postgresql*, for example.

Likewise, we have:

``` haskell
type instance Operation Delete c a i r  = i      -> c -> IO (r Delete)
type instance Operation ListAll c a i r =           c -> IO [a]
type instance Operation Update c a i r  = i -> a -> c -> IO (r Update)
type instance Operation View c a i r    = i      -> c -> IO (Maybe a)
```

Equipped with what we've covered so far, we can define a `Resource` like the following:

``` haskell
-- with:
-- * add :: User -> Connection -> IO (PGResult Add)
-- * listAll :: Connection -> IO [User]
-- * view :: UserId -> Connection -> IO (Maybe User)

-- noCatch just doesn't watch for any exception
-- pgsqlcontext is something you can easily define
--   using servant-postgresql,
mkResource "users" pgsqlcontext noCatch
  & addWith Users.add
  & listAllWith Users.listAll
  & viewWith Users.view
```

And this should make a bit more sense than at the beginning of this article. (By the way, `(&)` is just reversed function application: `x & f = f x`.)

In particular, we see that our `add`, `listAll` and `view` functions indeed conform to the shape expected by the respective instances of the `Operation` type family.

Now, what type does this `Resource` have? Some type variables become concrete right from the start when calling `mkResource`, and some others only later, when adding support for operations.

- `c` and `e` from `Resource c a i r e ops` become fixed, concrete types when feeding `mkResource` with the context and the exceptions catcher.
- `a`, `i`, `r` become concrete types only when you add support for an operation that **actually** uses them, in their `Operation` instance.
- `ops` is deduced by looking at the final list of operations we've provided support for.

When we put this all together, we can deduce the type of our `"users"` resource to be `Resource Connection User UserId PGResult e [View, ListAll, Add]`. We're still polymorphic in the error type because our list of "catchers" is empty.

And... I'll be honest with you. `addWith`, `viewWith` and friends actually are all just one, identical function in disguise: `addOperation` from `Servant.Resource`.

``` haskell
-- | Add an operation to a resource by specifying the \"database function\"
--   that'll actually perform the lookup, update, listing, search and what not.
--
--   We statically enforce that the operation we're adding isn't
--   already supported by the 'Resource', when built with @ghc >= 7.8@.
addOperation :: Contains o ops ~ False
             => Operation o c a i r 
             -> Resource c a i r e ops 
             -> Resource c a i r e (o ': ops)
addOperation opfunc resource =
  resource { operations = Cons opfunc (operations resource) }
```

So if you define your own operations, e.g `Search`, you could just have `searchWith = addOperation` but refining the type by forcing `o` to be `Search` in the signature of `searchWith` and by replacing `Operation Search c a i r` by what it actually is. This all could look like:

``` haskell
data Search

type instance Operation Search c a i r =
  SearchQuery -> c -> IO (SearchResult a)

-- a search query
type SearchQuery = Text

-- an item and its "score" for the search
data SearchResult a = SR !Double a

-- we restrict the type of addOperation manually
-- but the implementation is really the same.
searchWith :: Contains Search ops ~ False
           => (SearchQuery -> c -> IO (SearchResult a))
           -> Resource c a i r ops
           -> Resource c a i r (Search ': ops)
searchWith = addOperation -- no need to think here. this just works.
```

And then you could do

``` haskell
mkResource "users" pgcontext noCatch
  -- ... some other operations ...
  & searchWith somefunction
```

provided `somefunction` has the appropriate type (`SearchQuery -> c -> IO (SearchResult a)`). This is really what defining a new "abstract" operation is all about in servant.

### Abstract... ?

Yes, so far we've seen how to describe resources, but we haven't really covered how to run them. There was this `runResource` thing at the beginning of the tutorial -- what is that? where does it live?

The answer is that a `Resource` is just a description, which means we yet have to "build something real" from it, something we can run, of particular relevance would be to be able to run it inside a web-service. And that's what `servant-scotty` is about.

## servant-scotty

We have defined a toy-resource, `"users"`. What now? Well, like I said at the beginning, we use [scotty](http://hackage.haskell.org/package/scotty) a lot at [Zalora](http://github.com/zalora) so right now the only way to "make a resource damn real for a moment" is to generate a scotty web-service from the decsription using the *servant-scotty* package, and in particular by importing the `Servant.Scotty.Prelude` module.

### Running a `Resource`

What should the meaning of "running a resource" be? *servant-scotty*'s take on that is: generate one (or more) endpoints for *every operation your `Resource` supports*.

More concretely, `Servant.Scotty` exports a `runResource` function which will set up endpoints for your operations. If you call `runResource` on a resource with an empty list of operation, it won't set up any endpoint. If you call `runResource` on a list of the form `o ': ops`, it will set up stuffs for `o` and then move on with the rest of the operations (`ops`). We have a class that drives this type-level recursion for us: `Runnable`, of which `runResource` is a member.

``` haskell
class Runnable ops where
  -- | Call this function to setup a 'Resource' in your
  --   scotty application.
  runResource :: {- some constraints ... -}
              => Resource c a i r e ops
              -> ScottyT e m ()
```

And we have instances of `Runnable` that will do the right thing and will only trigger if you've defined how to interpret each operation properly.

But what do "set up stuffs for `o`" or (equivalently) "interpret each operation" mean? Well, it depends on what `o` is, of course! If we want to list all our users for example, this should mean setting up a `get "/users"` endpoint that would use `Users.listAll` to get all users and e.g rely on a `ToJSON User` instance being there to turn this user list into a JSON array of users in JSON format.

This was just an example, but it demonstrates that running a resource amounts to running something for each operation. And that each operation has to specify *how it should be translated in terms that scotty can understand*. And this is exactly what the `ScottyOp` class (from `Servant.Scotty.Op`) captures, so let's take some time to get familiar with it.

### Running an operation: `ScottyOp`

Well, what better way to tackle this is there than looking at the code for `ScottyOp`.

``` haskell
-- | A class that lets you define one or more handler(s) for an operation @o@.
class ScottyOp o where
  -- | Each operation can define its own constraints on:
  --
  --   * the type of the entries, @a@
  --   * the type by which the entries are indexed, @i@
  --   * the result type @r@ of \"effectful\" database operations
  --     (those that add/update/delete entries)
  --
  --   This is useful because that way, your types will /only/ have to
  --   satisfy the constraints /specified/ by the operations your 'Resource'
  --   carries, not some global dumb constraints you have to pay for even if
  --   you don't care about the operation that requires this constraint.
  type Suitable o a i (r :: * -> *) :: Constraint

  -- | Given a 'Resource' and the \"database function\" (so to speak)
  --   corresponding to your operation, do some business in /scotty/'s
  --   'ScottyT' and 'ActionT' monads to define a handler for this very operation.
  --
  --   To provide the \"database function\" with some 'Context' @c@
  --   you can use 'Servant.Context.withContext' to run the operation
  --   and 'Servant.Resource.context' to get the context of your 'Resource'.
  --
  --   To catch exceptions around your db operation in your handler,
  --   you can use the 'Servant.Resource.excCatcher' getter to access the
  --   'Servant.Error.ExceptionCatcher' of your 'Resource' and
  --   'Servant.Error.handledWith' to catch them and convert them
  --   to your error type @e@. You can then 'raise' the error value
  --   if you have a sensible default handler or handle it locally and
  --   respond with whatever is appropriate in your case.
  runOperation :: (Functor m, MonadIO m, ScottyError e, Suitable o a i r)
               => Resource c a i r e (o ': ops)
               -> Operation o c a i r
               -> ScottyT e m ()
```

Alright, so first, we see that to define an instance of `ScottyOp`, we have to give an instance of `Suitable` for our operation, and that the result should be a [`Constraint`](https://www.haskell.org/ghc/docs/latest/html/users_guide/constraint-kind.html). Hmm...

#### Example: `ListAll`

Let's use our "list all" example again. If we were to define a scotty action triggered when someone issues a *GET* request to `/users` (or any other resource supporting the "list all" operation -- an operation doesn't, can't and shouldn't care about the concrete types of entries and index it deals with: it's really meant to synthesize some general pattern resulting from a mix of getting stuffs from the url, the request body, doing something with these, and then turning them back into a some appropriate response) that would do what we described above, there's just one thing we need, and we've already mentionned it: a `ToJSON` instance for the `a` type. So we could start writing an instance for `ListAll` right now:

``` haskell
instance ScottyOp ListAll where
  type Suitable ListAll a i r = ToJSON a
  -- ... to be continued ...
```

We're not indexing any entry here in the URL or something, so we don't care about some index of type `i` being fetchable from somewhere, and getting a listing isn't "an effectful operation", i.e one that modifies something in "the database", so we don't care about `r` either. All we need is that `ToJSON` instance, really.

Now we have to define a `runOperation` for the `ListAll` operation. When considered with `o = ListAll`, `runOperation`'s signature:

``` haskell
runOperation :: (Functor m, MonadIO m, ScottyError e, Suitable o a i r)
             => Resource c a i r e (o : ops)
             -> Operation o c a i r
             -> ScottyT e m ()
```

becomes

``` haskell
runOperation :: (Functor m, MonadIO m, ScottyError e, ToJSON a)
             => Resource c a i r e (ListAll : ops)
             -> (c -> IO [a])
             -> ScottyT e m ()
```

Well, that definitely seems doable now!

``` haskell
runOperation resource op =
  -- 1/ whatever our resource is, the listing will happen at
  --    /<resource name> on GET requests
  get (capture $ "/" ++ name res) $ do
    -- 2/ 'safely' runs an operation using the resource's context
    --    catching potential exceptions and turning them in your error type,
    --    then 'raise'-ing the error for your defaultHandler to... handle it.
    --    Why not just 'safely res op'? Because in other cases, we want to
    --    fetch the operation's arguments from the url or from the request
    --    body (which servant-scotty provides helpers for), so 'safely' takes
    --    an operation generated in scotty's ActionT monad. You'll understand
    --    this better when we'll cover the View operation in the next example.
    result <- safely res $ pure op
    -- 3/ respond relies on there being an instance of the 'Response' class
    --    which describes how we turn the result of a db operation into
    --    a proper response (a response body + an HTTP status code).
    --    In our case, we get back a list of entries, and there's an instance
    --    of Response for list of entries as long as the type of
    --    these entries has a ToJSON instance.
    --    But luckily, our 'Suitable ListAll' constraint
    --    requires precisely this! :-)
    respond result
```

And we're done. Now any `Resource` with a list-all operation can see that operation be turned into an endpoint that sends back the list of all entries in JSON. Note that this instance is already provided by *servant-scotty*, in `Servant.Scotty.Prelude`. Also, we'll get to know the `Response` class more intimately soon, don't worry. Without all the big comments, the code is incredibly short:

``` haskell
instance ScottyOp ListAll where
  type Suitable ListAll a i r = ToJSON a

  runOperation res op =
    get (capture $ "/" ++ name res) $ do
      result <- safely res $ pure op
      respond result
```

Nice! It's now time to see another exemple of `ScottyOp` instance, and one where we'll need to fetch an identifier from the URL.

#### Example: `View`

`ListAll` is quite easy. The answer to a request doesn't even require an argument. We'll now see what's different when there's some kind of indexing going on. Indeed, we're going to define what it means to `View` an entry.

First, let's remember what the "database operation" for viewing an entry should look like:

``` haskell
type instance Operation View c a i r = i -> c -> IO (Maybe a)
```

This time, we need to provide an index to the operation in addition to the context. But... when we define a `ScottyOp` instance we don't know (and don't care) what the entries we're dealing with are, it can be anything. So how are we supposed to fetch an index generically? *servant-scotty* provides something for this, the `Index` class from `Servant.Scotty.Arguments`.

``` haskell
-- | What it means for a scotty 'Resource'
--   to have an index type.
--
--   * 'idx' should lookup in the request path
--     whatever is necessary to get the @i@
--     of @Resource c a i r e ops@, for operations
--     that take it as an argument, e.g /Delete/,
--     /Update/ or /View/.
--
--   * 'route' should return a 'String' that'll be
--     passed to 'capture'. You may use one or more
--     \"path parameters\" (calls to 'param', instances
--     of 'Param') to compute your value of type @k@.
--     You probably want to use 'name' on the 'Resource'
--     to generate the beginning of the path.
class Index k where
  -- | Lookup the index in the request path
  idx :: (Functor m, MonadIO m, ScottyError e)
      => ActionT e m k

  -- | String to 'capture' that represents the
  --   'RoutePattern'.
  route :: Resource c a k r e ops -> String
```

So if we constrain `i` to have an `Index` instance, we can rely on being able to automatically lookup an `i` argument for our database operations. Nice! That's all we need to `View` an entry. Regarding the response, even if you're not forced to use it, *servant-response* does provide an instance that can turn a `Maybe a` result into a "smart" JSON response that just prints the value as JSON if it's found, or prints

``` javascript
{ "message" : "not found" }
```

if not. This of course means this instance can be picked only when your entry type as a `ToJSON` instance.

The `route` method will also be helpful, it lets you define just the piece of the route pattern that references an identifier, so that you can use it when defining your endpoint. It takes a `Resource` as a dummy argument to let the compiler know which `i` we want the route pattern for.

Equipped with this, we can define a `ScottyOp` instance for `View`.

``` haskell
instance ScottyOp View where
  type Suitable View a i r =
    (Index i, ToJSON a)

  runOperation res op =
    -- could be:    /     users       /:username
    get (capture $ "/" ++ name res ++ route res) $ do
      -- we use <$> (infix fmap) syntax to feed the operation with the index's
      -- value and then run the database operation with 'safely' like before
      result <- safely res $ op <$> idx
      respond result
```

Note that independently from the `Index` class, *servant-scotty* also provides a small combinator from trying to decode the request's body as JSON.

``` haskell
-- | Simply gets the request's body as JSON
--   (or raises an exception if the decoding fails)
js :: (MonadIO m, ScottyError e, FromJSON a)
   => ActionT e m a
js = jsonData

-- js stands for json, but scotty has a 'json' function
-- so i named it 'js'.
```

This is useful for the `Update` operation which needs an index *and a new value for your entry*. You could look them up both by simply doing

``` haskell
op <$> idx <*> js
```

I think that by now you have a good intuition of what's going on in *servant-scotty* except on the bits related to how we generate a response from the result of an operation, centered around the `Response` class, so let's take a look at it.

## servant-response

This package is really small and simple. It contains the `Response` typeclass in [`Servant.Response`](http://alpmestan.com/servant/servant-response/Servant-Response.html) and it has a [`Servant.Response.Prelude`](http://alpmestan.com/servant/servant-response/Servant-Response-Prelude.html) module that reexports the class in addition to some standard response types you may want to (re)use -- they already are used by the standard operations `Add`, `Delete` etc in their `ScottyOp` instances.

This packages could have been named *servant-json-response*. I'm pretty sure we could work out a generic machinery for turning results into responses in an output format agnostic way, but right now this would be overkill, we only use JSON services here at Zalora, for now.

Anyway, here's the `Response` class.

``` haskell
-- | A class that ties return types of your database operations
--   and the output that will be generated to communicate
--   the result.
--
-- * The first type, @resp@, is the response type that will be encoded
--   in JSON and sent as the response body.
--
-- * The second type, @result@, is the result type of your \"database\"
--   or \"context\" operation.
--
--   For example, if you're adding an item, and if you're using
--   postgresql-simple, you'll probably want to use the
--   'Response' instances defined in the servant-postgresql package,
--   in the @Servant.PostgreSQL.Prelude@ module.
--
--   It lets you specify, given a value of your result, if no
--   exception is thrown, what response should be sent as JSON
--   to the client along with what HTTP status.
--
--   There's a functional dependency at play: the result type
--   of a database operation determines the representation that'll be
--   picked for generating the json output.
class ToJSON resp => Response resp result | result -> resp where
  toResponse :: result -> (resp, Status)
```

So, given a result, decide on what response body and HTTP status should be sent back to the client.

The package also provides a couple of standard response types you can use, with their JSON instances, in [`Servant.Response.Prelude`](http://alpmestan.com/servant/servant-response/Servant-Response-Prelude.html). Please see the haddocks for that module if you want to read more about that.

While that's about it for the *servant-response* package, you can find a couple more instances of `Response` in *servant-postgresql*, which is presented below, to generate values of the standard responses from the prelude module from results of queries executed with *postgresql-simple*.

## servant-postgresql

### Creating a PostgreSQL `Context`

In the [`Servant.Context.PostgreSQL`](http://alpmestan.com/servant/servant-postgresql/Servant-Context-PostgreSQL.html) module, you can find helper functions to easily get a `Context` suitable for running queries on a PostgreSQL server.

There are two types of `Context` you can produce:

- Using `contextOfConnInfo` or `contextOfConnStr`, you get a context where each request fires up a new connection and runs your query.
- Using `pooledContextOfConnInfo` or `pooledContextOfConnStr`, you get a context that uses a pool of connection (based on [resource-pool](http://hackage.haskell.org/package/resource-pool)).

The `Context` returned by these functions is then meant to be given as an argument to `mkResource` when declaring your `Resource`.

The pooling support is based on the *servant-pool* package, which will be described toward the end of this tutorial.

### Generating responses

Let's say you have the following function.

``` haskell
addUser :: User -> Connection -> IO Int64
addUser user conn =
  execute conn "INSERT INTO users(username, password) VALUES (?, ?)" (username user, password user)
```

All you need to change to let *servant* handle the response generation for you here is to add a call to `pgresultOfInt64` and `PGResult` with `Add` in the return type.

``` haskell
addUser :: User -> Connection -> IO (PGResult Add)
addUser user conn = pgresultOfInt64 $
  execute conn "INSERT INTO users(username, password) VALUES (?, ?)" (username user, password user)
```

This will allow the conversion of the `PGResult` to `[UpdateResponse](http://alpmestan.com/servant/servant-response/Servant-Response-Prelude.html#t:UpdateResponse) Add` that can then be sent as JSON to the client. See the haddocks for more detail about this.

Why do we tag `PGResult` with the operation type? Because that lets us behave differently when we generate the response depending on whether we're adding, updating or deleting an entry for example. The latter two may respond with status code `404` if the specified identifier couldn't be found, and the former should answer with HTTP status code `201` if the entry was successfully created. This is what happens with the `Response` instances of `PGResult` for these 3 operations, in `Servant.PostgreSQL.Prelude`.

## servant-pool

The pooling support from *servant-postgresql* is defined using handy functions from this package. It's basically a tiny wrapper on top of [resource-pool](http://hackage.haskell.org/package/resource-pool) that lets you easily produce a `Context` that uses a pool of whatever your "connection-ish" thing is. There are two ways to do that.

- `pooledContext` creates a `Pool` for you and turns it into a context, but...
- if you already have a `Pool` around, you can just call `contextOfPool` to turn it into a `Context`.

These functions can come handy to you in case you want to use a pool of connections to a MySQL server or any kind of database in general, for example.

See the haddocks for these functions [here](http://alpmestan.com/servant/servant-pool/Servant-Context-Pool.html).
