servant
=======

**servant** is at its core a type-safe library for defining
REST-y `Resource`s just from a set of "database operations". Indeed, let's imagine you have the following type and functions:

``` haskell
data Person =
  Person { firstName :: Text
         , lastName  :: Text
         , age       :: Text
         }

-- 'Connection' below refers to postgresql-simple's Connection type
-- and [Only Int] is the return type of a call to postgresql-simple's
-- 'execute' function
addPerson :: Connection -> Person -> IO [Only Int]
listPersons :: Connection -> IO [Person]
```

Describing a simplistic `Resource` that supports adding entries
and listing them all amounts to:

``` haskell
import Servant.Error
import Servant.PostgreSQL
import Servant.Resource

pgConnString :: ByteString
pgConnString = "" -- replace with your connection string

personResource =
  mkResource "persons" noCatch (contextOfConnStr pgConnString)
    & addWith addPerson
    & listAllWith listPersons
```

where `noCatch` just means you'll ignore all exceptions when running your "database operations" and where the `contextOfConnStr` call just sets up a function that'll fire up a connection whenever an operation will be performed.

You could easily switch that to a connection pool, see the documentation of the `Servant.PostgreSQL` module as well as the `servant-pool` package. It's as simple as changing the call to `contextOfConnStr`. Instead you'd call a function that sets up a "pooled context", from that same module, named `pooledContextOfConnStr`. It takes various parameters for tweaking the pool to your needs.

And now, we can turn that `personResource` into a `scotty` web-service by simply doing:

``` haskell
import Servant.Scotty
import Web.Scotty

port :: Int
port = 4321

main :: IO ()
main =
  -- the service will listen on port 4321
  scotty port (runResource personResource)
```

## Extensibility

I've rewritten `servant` a couple of times because there's no point
in tying it to a particular web framework or database backend (and it *was* tied to scotty, in previous rewrites). I've
kept the type tricks but worked hard enough to completely separate
the *resource description* bits from the parts that actually are about
setting up a webservice just from a description. 

In addition to the RESTish operations, you can define your own in a simple
and type-safe way.

Also, right now, I'm focusing on our use cases at **Zalora**, where we need
pooling support on top of /PostgreSQL/ connections, and tend to write web apps using [scotty](http://hackage.haskell.org/package/scotty). Nothing keeps you from writing a "backend" for your favorite web framework, it would most likely be quite similar to the scotty one, so you can just look at its code. However, if I don't have a write up yet about how that should be done by the time you want to do that, I gladly encourage you to shoot me an email.

## Documentation

[Documentation with all modules all packages](http://alpmestan.com/servant/)

or for individual packages:

- [servant](http://alpmestan.com/servant/servant/)
- [servant-pool](http://alpmestan.com/servant/servant-pool/)
- [servant-postgresql](http://alpmestan.com/servant/servant-postgresql/)
- [servant-scotty](http://alpmestan.com/servant/servant-scotty/)
