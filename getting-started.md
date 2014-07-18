# Introduction

If you've seen [the README](https://github.com/zalora/servant/blob/master/README.md) before landing here, you've probably seen:

> **servant** is at its core a type-safe and *very extensible* library for defining REST-y webservices around some core "database operations", in just a few very simple lines, e.g:
>
> ``` haskell
> mkResource "users" pgsqlcontext pgexceptions
>   & addWith Users.add
>   & viewWith Users.view
>   & deleteWith Users.delete
> ```
>
> which can then be turned into a [scotty](http://hackage.haskell.org/package/scotty) webservice by calling `runResource` (from *servant-scotty* -- the only backend we have for now) on it.

Now this isn't the most useful sample of code, so the goal in this document is just to write a small webservice around some `User` data type, that'll let us add, delete and list `User`s. Note that the example program we'll write is available in this repository, in the [`servant-example/`](https://github.com/zalora/servant/tree/master/servant-example) directory, as a cabalized and runnable project along with some SQL code to run to setup a postgresql table.
