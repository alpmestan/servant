{-# LANGUAGE DataKinds,
             OverloadedStrings #-}
module Main where

import Data.Monoid
import Database.PostgreSQL.Simple (Connection)
import Servant.PostgreSQL.Prelude
import Servant.Scotty
import Servant.Scotty.Prelude

import Error
import Model.User

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

users :: Context Connection
      -> Resource Connection
                  User
                  Email
                  PGResult
                  ServiceError
                  '[ListAll, Delete, Add]
users ctx =
  mkResource "users" ctx exceptions
    & addWith     addUser
    & deleteWith  deleteUser
    & listAllWith listUsers

  where exceptions = sqlerrorCatcher <> violationsCatcher

getContext :: IO (Context Connection)
getContext = 
  pooledContextOfConnStr 10 -- number of "sub-pool"
                         10 -- keep idle connections during 10 secs
                         100 -- max. 100 open resources per sub-pool
                         connstring
  where connstring = "host=localhost user=alp"
