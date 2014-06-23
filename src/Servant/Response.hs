{-# LANGUAGE DataKinds,
             DeriveGeneric,
             FlexibleInstances,
             KindSignatures,
             OverloadedStrings,
             MultiParamTypeClasses,
             FunctionalDependencies #-}

module Servant.Response where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Types.Status

data UpdateResponse =
  UpdateResponse { success :: !Bool 
                 , msg     :: !Text
                 }
  deriving (Eq, Show, Generic)

instance ToJSON UpdateResponse where

data LookupResponse a =
    NotFound
  | Found a
  deriving (Eq, Show)

instance ToJSON a => ToJSON (LookupResponse a) where
  toJSON NotFound  = object [ "message" .= ("Not found" :: Text) ]
  toJSON (Found x) = toJSON x

-- Response class

class ToJSON resp => Response resp result | result -> resp where
  toResponse :: result -> (resp, Status)





