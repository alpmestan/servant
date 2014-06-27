{-# LANGUAGE DeriveGeneric,
             OverloadedStrings,
             MultiParamTypeClasses #-}
{- |
Module      :  Servant.Response
Copyright   :  (c) Zalora SEA 2014
License     :  BSD3
Maintainer  :  Alp Mestanogullari <alp@zalora.com>
Stability   :  experimental

This module contains a generic 'Response' class for tying the result
of some \"database computation\" to some response type of yours,
or to some of the default ones from this module, namely 'LookupResponse'
and 'UpdateResponse'.

-}
module Servant.Response
  ( -- * The 'Response' class
    Response(..)

  , -- * Useful generic response types
    UpdateResponse(..)
  , LookupResponse(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Types.Status

-- | A generic response type for any "effectul" operation
--   on a 'Resource', like adding, updating or deleting an item.
--
--   It simply holds a 'Bool' that indicates whether the operation
--   was successful or not, and if it wasn't, it'll embed a text
--   describing what went wrong.
--
--   You can of course skip this one and use a more appropriate
--   for your particular application.
data UpdateResponse =
  UpdateResponse { success :: !Bool 
                 , msg     :: !Text
                 }
  deriving (Eq, Show, Generic)

instance ToJSON UpdateResponse where

-- | A generic response type for an operation performing
--   some kind of (potentially failing) lookup of an item
--
--   This is useful when writing a web application, where you
--   want to send for example a JSON message saying the item wasn't found
--   along with status 404 when the item isn't found, but just send the item
--   if it could be found. This is (purposefully) isomorphic to 'Maybe'.
data LookupResponse a =
    NotFound
  | Found a
  deriving (Eq, Show)

instance ToJSON a => ToJSON (LookupResponse a) where
  toJSON NotFound  = object [ "message" .= ("Not found" :: Text) ]
  toJSON (Found x) = toJSON x

-- | A class that ties return types of your database operations
--   and the output format that will be used to communicate
--   the result. If you're adding an item, and if you're using
--   postgresql-simple, you'll probably want to write an instance like:
--
-- > instance Response UpdateResponse [Only Int] where
--
--   because @[Only Int]@ is what postgresql-simple's @execute@ returns.
--
--   It lets you specify, given a value of your result, if no
--   exception is thrown, what response should be sent as JSON
--   to the client along with what HTTP status.
class ToJSON resp => Response resp result where
  toResponse :: result -> (resp, Status)

instance ToJSON a => Response (LookupResponse a) (Maybe a) where
  toResponse Nothing  = (NotFound, status404)
  toResponse (Just v) = (Found v, status200)
