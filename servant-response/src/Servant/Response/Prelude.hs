{-# LANGUAGE DeriveGeneric, MultiParamTypeClasses, OverloadedStrings #-}
module Servant.Response.Prelude
  ( -- * 'Response' class
  	module Servant.Response
  , -- * Useful reusable types and instances
    UpdateResponse(..)
  , LookupResponse(..)
  ) where

import Data.Aeson
import Data.Text
import GHC.Generics
import Network.HTTP.Types.Status
import Servant.Response

-- | A generic response type for any "effectul" operation
--   on a 'Resource', like adding, updating or deleting an item.
--
--   It simply holds a 'Bool' that indicates whether the operation
--   was successful or not, and if it wasn't, it'll embed a text
--   describing what went wrong and is meant to be tagged
--   (see the @o@ type parameter) with the operation it's associated to.
--
--   This lets us have different instances for the standard
--   /Add/ and /Update/ operations for example, where the former
--   should respond with HTTP status code /201/ if the entry was created,
--   whereas the latter should just use status code /200/.
--
--   You can of course skip this one and use a more appropriate
--   for your particular application.
data UpdateResponse o =
  UpdateResponse { success :: !Bool 
                 , msg     :: !Text
                 }
  deriving (Eq, Show, Generic)

-- | e.g:
--
-- > { "success" : false, "msg" : "couldn't add item: blabla"}
instance ToJSON (UpdateResponse o) where

-- | A generic response type for an operation performing
--   some kind of (potentially failing) lookup of an item
--
--   This is useful when writing a web application, where you
--   want to send for example a JSON message saying the item wasn't found
--   along with status 404 when the item isn't found, but just send the item
--   if it could be found. This is (purposefully) isomorphic to 'Maybe'.
data LookupResponse a =
    NotFound
  | Found !a
  deriving (Eq, Show)

-- | If you have some type convertible to JSON,
--   you can wrap it in 'LookupResponse' whenever you are
--   looking up a value associated to some identifier
--   where the lookup may fail.
--   It'll send the JSON-encoded value if found or
--
-- > { "message" : "Not found" }
--
--   if not found. This makes sure you send /valid/
--   JSON through the wires even when the target doesn't exist.
instance ToJSON a => ToJSON (LookupResponse a) where
  toJSON NotFound  = object [ "message" .= ("Not found" :: Text) ]
  toJSON (Found x) = toJSON x

-- | Make 'LookupResponse' a proper 'Response' for
--   'Service.Context.Context' lookups returning a 'Maybe' value,
--   returning 404 when Nothing is returned, along with a not found
--   message in json. Used by 'View'.
instance ToJSON a => Response (LookupResponse a) (Maybe a) where
  toResponse Nothing  = (NotFound, status404)
  toResponse (Just v) = (Found v, status200)

-- | Just send the list of entries as a JSON array,
--   with status code 200. Used by 'ListAll'.
instance ToJSON a => Response [a] [a] where
  toResponse list = (list, status200)
