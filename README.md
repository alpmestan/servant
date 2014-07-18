servant
=======

**servant** is at its core a type-safe and *very extensible* library for defining REST-y webservices around some core "database operations", in just a few very simple lines, e.g:

``` haskell
mkResource "users" pgsqlcontext pgexceptions
  & addWith Users.add
  & viewWith Users.view
  & deleteWith Users.delete
```

which can then be turned into a [scotty](http://hackage.haskell.org/package/scotty) webservice by calling `runResource` (from *servant-scotty* -- the only backend for now) on it.

## Documentation

- [Getting started with servant](https://github.com/zalora/servant/blob/master/getting-started.md)
- [Tutorial](https://github.com/zalora/servant/blob/master/tutorial.md)
- [Haddocks for all the modules, from all packages](http://alpmestan.com/servant/)
- Haddocks of individual packages:
    - [servant](http://alpmestan.com/servant/servant/)
    - [servant-pool](http://alpmestan.com/servant/servant-pool/)
    - [servant-postgresql](http://alpmestan.com/servant/servant-postgresql/)
    - [servant-response](http://alpmestan.com/servant/servant-response/)
    - [servant-scotty](http://alpmestan.com/servant/servant-scotty/)

## Extensibility

I've rewritten `servant` a couple of times because there's no point
in tying it to a particular web framework or database backend (and it *was* tied to scotty, in previous rewrites). I've
kept the type tricks but worked hard enough to completely separate
the *resource description* bits from the parts that actually are about
setting up a webservice just from a description or turning results into proper json responses and what not. 

In addition to the standard REST operations provided through the various `Prelude` modules in the servant packages, you can define your own in a simple
and type-safe way.

Also, right now, I'm focusing on our use cases at **Zalora**, where we need
pooling support on top of /PostgreSQL/ connections, and tend to write web apps using [scotty](http://hackage.haskell.org/package/scotty). Nothing keeps you from writing a "backend" for your favorite web framework, it would most likely be quite similar to the scotty one, so you can just look at its code. However, if I don't have a write up yet about how that should be done by the time you want to do that, I gladly encourage you to shoot me an email. The following documentation links should however help a lot.
