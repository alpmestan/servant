# servant

[![Build Status](https://secure.travis-ci.org/alpmestan/servant.svg)](http://travis-ci.org/alpmestan/servant)

![servant](https://raw.githubusercontent.com/alpmestan/servant/new-impl/servant.png)

These libraries provides a family of combinators to define webservices and automatically generate the documentation and client-side querying functions for each endpoint.

In order to minimize the dependencies depending on your needs, we provide these features under different packages.

- `servant`, which contains everything you need to *declare* a webservice and *implement* an HTTP server with handlers for each endpoint.
- `servant-client`, which lets you derive automatically Haskell functions that let you query each endpoint of a *servant* webservice.
- `servant-docs`, which lets you generate API docs for your webservice.
- `servant-jquery`, which lets you derive Javascript functions (based on jquery) to query your API's endpoints, in the same spirit as `servant-client`.

## Haddocks

We try to maintain up-to-date docs:

- [servant](http://alpmestan.github.io/servant/servant/)
- [servant-client](http://alpmestan.github.io/servant/servant-client/)
- [servant-docs](http://alpmestan.github.io/servant/servant-docs/)
- [servant-jquery](http://alpmestan.github.io/servant/servant-jquery/)

