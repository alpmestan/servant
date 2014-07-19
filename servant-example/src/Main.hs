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
  ctx <- getContext
  scottyT 4321 id id $ do
    defaultHandler $ \err -> do
      status badRequest400
      json err
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
