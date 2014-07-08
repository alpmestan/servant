{-# LANGUAGE DataKinds,
             OverloadedStrings,
             FlexibleInstances,
             MultiParamTypeClasses #-}
module Main where

import Data.Int
import Data.Monoid
import Database.PostgreSQL.Simple (Connection)
import Servant.Error
import Servant.Context.PostgreSQL
import Servant.Scotty
import Servant.Scotty.Prelude
import Web.Scotty.Trans

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
                  Int64
                  ServiceError
                  '[ListAll, Add]
users ctx =
  mkResource "users" ctx exceptions
    & addWith     addUser
    & listAllWith listUsers

  where exceptions = sqlerrorCatcher <> violationsCatcher

getContext :: IO (Context Connection)
getContext = 
  pooledContextOfConnStr 10 -- number of "sub-pool"
                         10 -- keep idle connections during 10 secs
                         100 -- max. 100 open resources per sub-pool
                         connstring
  where connstring = "host=localhost user=alp"

instance Response (UpdateResponse Add) Int64 where
  toResponse n = (resp, statuscode)
    where resp = UpdateResponse successful err
          successful = n > 0
          err = if n > 0 then "" else "no affected row"
          statuscode = if n > 0 then status201 else status400

