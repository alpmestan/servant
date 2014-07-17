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

I've been talking about operations so far without saying much about them, except that at its core an operation is juste a type, which usually is empty. The standard operations provided by *servant* actually **are** empty types:

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
    -- 2/ safely runs an operation using the resource's context
    --    catching potential exceptions and turning them in your error type.
    --    Why not just 'safely res op'? Because in other cases, we want to
    --    fetch the operation's arguments from the url or from the request
    --    body (which servant-scotty provides helpers for), so 'safely' takes
    --    an operation generated in scotty's ActionT monad. You'll understand
    --    this when we'll cover the View operation.
    result <- safely res $ pure op
    -- 3/ respond relies on there being an instance of the 'Response' class
    --    which describes how we turn the result of a db operation into
    --    a proper response (a response body + an HTTP status code).
    --    In our case, we get back a list of entries, and there's an instance
    --    for list of entries as long as the type of these entries has
    --    as ToJSON instance. But luckily, our 'Suitable ListAll' constraint
    --    requires precisely this! :-)
    respond result
```
