% Rethinking webservices in Haskell with servant

# A little bit of history

## Prologue

- For our respective jobs, we needed to be able to spin up quite a lot of webservices, worrying as little as possible about the plumbing
- We needed support for
    - url captures: `/users/:userid` would accept `/users/23` and pass `23` to the handler
    - get parameters: `/users?registered_since=<date>`, passing the date (if any is given) to the handler
    - request body extraction whenever we need it
- A lot of the web frameworks/libraries were too heavy and really more adapted for "big website" development
- Silk's [rest](http://github.com/silkapp/rest) is nice, but unsatisfactory to us in terms of minimalism: we wanted to get the boilerplate as close to 0 as possible

## servant 0.1

- We initially turned to [scotty](http://hackage.haskell.org/package/scotty), because it's minimal while providing everything we needed...
- ...and expressed combinations of "get this GET param", "get the value for this url capture", "get the request body" and what not as reusable operations
- We could abstract away a lot of the boilerplate, e.g reacting to a missing request body on an endpoint that expects some JSON object there, same for GET parameters and url captures.
- While there were some nice benefits with this approach, we still had to do a fair amount of plumbing, and it just didn't felt "natural", straighforward.
- In addition to that, we secretly wanted to reflect all these operations in the types, in order to generate client-side code and documentation automatically. And with `servant-0.1`, that was not possible.

> Still not satisfactory.

## Aha! moment

- What if we could express an API as a (admittedly big) type by composing features together, explicitly mentioning URL captures, GET parameters, request bodies and response types?
- This would in spirit be quite similar to parser combinators à-la [parsec](http://hackage.haskell.org/package/parsec), just at the type level instead -- otherwise we can't exploit the same code for generating a server and the client functions.
- This would make data dependency explicit for each server-side handler
- Combinator-based design usually works well.

> Hmm... Promising.

## The result: webservice API combinators (1)

- A type for each request method. `Get a` means it'll match HTTP GET requests and respond with an `a` encoded as JSON. Same for `Delete`, `Post` and `Put`.

    ``` haskell
    -- an API of a single endpoint, that matches GET request to /
    -- and returns a list of users encoded in JSON
    type UserAPI = Get [User]
    ```

- Use type-level string literals for static fragments of the path and chain literals or other combinators using a simple operator. We named ours `:>.

    ``` haskell
    type UserAPI = "users" :> Get [User]
    ```

- Most of the time, `:>` can be read as `/`:

    ``` haskell
    -- can be read as: GET /users/list-all
    type UserAPI = "users" :> "list-all" :> Get [User]
    ```

## The result: webservice API combinators (2)

- Alternative routes/endpoints, expressed as `route1 :<|> route2` in servant, work like `mplus`/`<|>` for parsers: try to match against the first route and if it fails, try the other.

    ``` haskell
    type UserAPI = "users" :> "list-all" :> Get [User]
              :<|> "users" :> "top-ten" :> Get [User]
    ```

- Explicitly mention URL captures in the type with `Capture`.

    ``` haskell
    -- equivalent to: GET /users/:userid in some web frameworks
    type UserAPI = "users" :> Capture "userid" Int64 :> Get User
    ```

- Explicitly expect a (typed) request body with `ReqBody`.

    ``` haskell
    -- equivalent to: POST /users
    -- with a JSON-encoded User in the request body
    -- and which responds with that same User value if successful
    type UserAPI = "users" :> ReqBody User :> Post User
    ```

## The result: webservice API combinators (3)

- Declare a standard query string parameter (which are *de-facto* optional) with `QueryParam`

    ``` haskell
    -- GET /users/search?q=john
    -- would search for users with "john" in their first or last name
    type UserAPI = "users" :> "search" :> QueryParam "q" Text :> Get [User]
    ```

- Declare a value-less query string parameter, acting like a `Bool`, with `QueryFlag`

    ``` haskell
    -- GET /users to get all the users
    -- GET /users?active to only get the active ones
    type UserAPI = "users" :> QueryFlag "active" :> Get [User]
    ```

- Declare a list-like query string parameter, like `foo` in `?foo[]=1&foo[]=2`, with `QueryParams` (notice the final `s`)

    ``` haskell
    -- GET /users?knows[]=haskell&knows[]=rust
    -- to get all the users who know Haskell and Rust
    type UserAPI = "users" :> QueryParams "knows" Text :> Get [User]
    ```

## The result: webservice API combinators (4)

- You can plug any [WAI](http://hackage.haskell.org/package/wai) app in your API. This is done by using `Raw` instead of `Get a` or the likes. For instance, we allow static file serving as a `Raw` endpoint. If the route leading to the `Raw` app has static strings in it, it is stripped off the request path sent to the WAI app: the app can just pretend it's running on `/` while being exposed under `/one/very/long/path`.

- Even better: you can write your own combinators! `Get` from servant is just an empty type:

    ``` haskell
    data Get a
    ```

- Everything `Get` does is actually defined in class instances: one for server-side behavior, one for (haskell) client-side querying functions, one for documentation generation and one for generating querying functions in javascript.

- This design means you can extend the servant vocabulary by defining your own types and writing instances for the existing classes for them...

- ... or extend servant's reach by defining a new class and then interpreting all the existing combinators in a way that's specific to your use case through instances of your class.

# Back to real life: writing an actual webservice

## Concrete example: managing books

We are going to write a (JSON) webservice to manage books, with 2 endpoints:

- `GET /books` to list all the books we have
- `POST /books` to add a new book, by sending a JSON representation of it in the request body.

We'll store the data in a PostgreSQL database.

## Book data type

``` haskell
data Book = Book
  { title  :: Text
  , author :: Text
  } deriving Generic

-- JSON instances
instance FromJSON Book
instance ToJSON Book

-- PostgreSQL instances
instance FromRow Book where
  fromRow = Book <$> field <*> field

instance ToRow Book where
  toRow book = [ toField (title book)
               , toField (author book)
               ]
```

## The API

Nothing too fancy here, just using the combinators we saw earlier.

``` haskell
             -- we explicitly say we expect a request body,
             -- of type Book
type BookApi = "books" :> ReqBody Book :> Post Book  -- POST /books
          :<|> "books" :> Get [Book]                 -- GET /books
```

## Server-side handlers

- `POST /books`: we insert the book found in the request body in our table
- `GET /books`: we just retrieve all the books from our table
- We glue the handlers with `:<|>`, just like in the type.

``` haskell
-- Let's stick the API type again here:
type BookApi = "books" :> ReqBody Book :> Post Book  -- POST /books
          :<|> "books" :> Get [Book]                 -- GET /books

server :: Connection -> Server BookApi
server conn = postBook
         :<|> getBooks

  where -- the aforementioned 'ReqBody' automatically makes this handler
        -- receive a Book argument
        postBook book = liftIO $ execute conn "insert into books values (?, ?)" book >> return book
        getBooks      = liftIO $ query_ conn "select * from books"
```

Now this is interesting! Let's zoom in on this server function, piece by piece.

## Server-side handlers (2)

``` haskell
server :: Connection -> Server BookApi
```

Assuming it is given a PostgreSQL connection, our `server` function returns something of type `Server BookApi`. `Server` is a type family in servant which expresses the type that the handlers when glued together should have. It basically makes sure you have an handler for each endpoint and that each handler's type matches the "type" given by the endpoint.

``` haskell
server conn = postBook
         :<|> getBooks
```

One handler for each endpoint, glued together with `:<|>`, just like in the API's type.

``` haskell
-- corresponds to: "books" :> ReqBody Book :> Post Book
postBook :: Book -> EitherT (Int, String) IO Book
postBook book = liftIO $ execute conn "insert into books values (?, ?)" book >> return book
```

The `ReqBody` bit of the endpoint gets automatically turned into an argument for the handler. The only thing that's required for this to work is a `FromJSON` instance for `Book`. From there, we just run a SQL query and return our `book` argument, and then use `liftIO`. This is necessary because server-side handlers live in the `EitherT (Int, String) IO` monad. The `Left` case is meant to store failure information, `Int` for the status and `String` for the message. This will probably change to a more robust representation soon.

## Server-side handlers (2)

``` haskell
-- corresponds to: "books" :> Get [Book]
getBooks :: EitherT (Int, String) IO [Book]
getBooks = liftIO $ query_ conn "select * from books"
```

`getBooks` doesn't receive any argument, because there's no `ReqBody`, `Capture` or `QueryParam` in the type of the corresponding endpoint. Had the type been

``` haskell
"books" :> QueryParam "author" Text :> Get [Book]
```

then servant would force the following type for `getBooks`:

``` haskell
getBooks :: Maybe Text -> EitherT (Int, String) IO [Book]
```

where the argument would be `Nothing` when no `author` parameter is passed in the query string or `Just authorname` when there's one.

## Running the server

``` haskell
bookApi :: Proxy BookApi
bookApi = Proxy

main = do
  conn <- connectPostgreSQL "host=localhost user=bookstore dbname=bookstore"
  run 8080 (serve bookApi $ server conn)

-- 'run' comes from Network.Wai.Handler.Warp
-- 'serve' comes from Servant
```
