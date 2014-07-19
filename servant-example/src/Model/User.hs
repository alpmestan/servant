{-# LANGUAGE DeriveGeneric, TypeSynonymInstances, OverloadedStrings #-}
module Model.User where

import Control.Applicative
import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Servant.PostgreSQL.Prelude
import Servant.Scotty.Prelude

type Email = Text

instance Index Email where
  -- To fetch the email that references an User
  -- we just look up the value of the "email" parameter
  -- in the request path.
  --
  -- E.g, if we issue the following request:
  --   DELETE /users/alp@blah.com
  -- and given this route pattern:
  --   /users/:email
  -- we want to lookup "alp@blah.com"
  idx = param "email"

  -- A "part of a route pattern" we can just reuse whenever we
  -- need to generate and endpoint that requires looking up
  -- parameters in the request path.
  route _ = "/:email"

data User =
  User { email :: Email
       , karma :: Int
       } deriving (Eq, Show, Generic)

instance FromRow User where
  fromRow = User <$> field <*> field

instance ToJSON User where
instance FromJSON User where

addUser :: User -> Connection -> IO (PGResult Add)
addUser user conn = pgresultOfInt64 $
  execute conn "insert into users(email, karma) values (?, ?)"
               (email user, karma user)

deleteUser :: Email -> Connection -> IO (PGResult Delete)
deleteUser email conn = pgresultOfInt64 $
  execute conn "delete from users where email = ?" (Only email)

listUsers :: Connection -> IO [User]
listUsers conn =
  query_ conn "select email, karma from users"
