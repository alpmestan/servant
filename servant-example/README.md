servant-example
===============

An example to demonstrate how to use `servant`. Will be documented as a tutorial soon.

``` bash
$ psql -f setup.sql
$ cabal run
# in another terminal
$ curl http://localhost:4321/users
[]
$ curl -d '{ "email": "alp@zalora.com", "karma": 10 }' -X POST http://localhost:4321/users
{"success":true,"msg":""}                
$ curl http://localhost:4321/users
[{"email":"alp@zalora.com","karma":10}]
```