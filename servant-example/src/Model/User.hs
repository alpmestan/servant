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
  idx = param "email"

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
