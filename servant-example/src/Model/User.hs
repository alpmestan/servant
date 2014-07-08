{-# LANGUAGE DeriveGeneric, TypeSynonymInstances, OverloadedStrings #-}
module Model.User where

import Control.Applicative
import GHC.Generics
import Data.Aeson
import Data.Int
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Servant.Scotty.Arguments
import Web.Scotty.Trans

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

addUser :: User -> Connection -> IO Int64
addUser user conn =
  execute conn "insert into users(email, karma) values (?, ?)"
               (email user, karma user)

listUsers :: Connection -> IO [User]
listUsers conn =
  query_ conn "select email, karma from users"
