{-# LANGUAGE DeriveGeneric,
             TypeFamilies,
             MultiParamTypeClasses,
             OverloadedStrings #-}
{- |
Module      :  Servant.Scotty.Prelude
Copyright   :  (c) Zalora SEA 2014
License     :  BSD3
Maintainer  :  Alp Mestanogullari <alp@zalora.com>
Stability   :  experimental

Instances of 'ScottyOp' for the operations defined
in "Servant.Prelude", along with some reusable types
necessary for the instances.
-}
module Servant.Scotty.Prelude
  ( -- * Defining 'Resource's and standard operations
    module Servant.Prelude

  , -- * 'ScottyOp' class and standard operations implementations in scotty
    ScottyOp(..)

  , -- * Helpful types, functions, classes and instances
    --   for defining your own operations
    module Servant.Scotty.Arguments
  , module Servant.Scotty.Response
  , LookupResponse(..)
  , UpdateResponse(..)
  ) where

import Control.Applicative
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Servant.Prelude
import Servant.Scotty.Op
import Servant.Scotty.Response
import Servant.Scotty.Arguments
import Web.Scotty.Trans

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

-- | Generate a
--
--   > POST /:resourcename
--
--   handler for adding entries.
--
--   /Constraints on @a@, @i@ and @r@/:
--
--   > type Suitable Add a i r = (FromJSON a, Response (UpdateResponse Add) r)
instance ScottyOp Add where
  type Suitable Add a i r =
    (FromJSON a, Response (UpdateResponse Add) r)

  runOperation res op =
    post (capture $ "/" ++ name res) $ do
      result <- safely res $ op <$> js
      respond result

-- | Generate a
--
--   > DELETE /:resourcename/<index specific stuffs>
--
--   handler for deleting entries.
--
--   /Constraints on @a@, @i@ and @r@/:
--
--   > type Suitable Delete a i r = (Index i, Response (UpdateResponse Delete) r)
instance ScottyOp Delete where
  type Suitable Delete a i r =
    (Index i, Response (UpdateResponse Delete) r)

  runOperation res op =
    delete (capture $ "/" ++ name res ++ route res) $ do
      result <- safely res $ op <$> idx
      respond result

-- | Generate a
--
--   > GET /:resourcename
--
--   handler for listing all entries.
--
--   /Constraints on @a@, @i@ and @r@/:
--
--   > type Suitable ListAll a i r = ToJSON a
instance ScottyOp ListAll where
  type Suitable ListAll a i r = ToJSON a

  runOperation res op =
    get (capture $ "/" ++ name res) $ do
      result <- safely res $ pure op
      respond result

-- | Generate a
--
--   > PUT /:resourcename/<index specific stuffs>
--
--   handler for updating an entry.
--
--   /Constraints on @a@, @i@ and @r@/:
--
--   > type Suitable Update a i r = (Index i, FromJSON a, Response (UpdateResponse Update) r)
instance ScottyOp Update where
  type Suitable Update a i r =
    (Index i, FromJSON a, Response (UpdateResponse Update) r)

  runOperation res op =
    put (capture $ "/" ++ name res ++ route res) $ do
      result <- safely res $ op <$> idx <*> js
      respond result

-- | Generate a
--
--   > GET /:resourcename/<index specific stuffs>
--
--   handler for viewing an entry.
--
--   /Constraints on @a@, @i@ and @r@/:
--
--   > type Suitable View a i r = (Index i, ToJSON a)
instance ScottyOp View where
  type Suitable View a i r =
    (Index i, ToJSON a)

  runOperation res op =
    get (capture $ "/" ++ name res ++ route res) $ do
      result <- safely res $ op <$> idx
      respond result
