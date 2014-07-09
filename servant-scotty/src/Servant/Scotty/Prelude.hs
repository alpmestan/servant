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

  , -- * Standard response types
    module Servant.Response.Prelude

  , -- * 'ScottyOp' class and standard operations implementations in scotty
    ScottyOp(..)

  , -- * Helpful types, functions, classes and instances
    --   for defining your own operations
    module Servant.Scotty.Arguments
  , module Servant.Scotty.Response
  , module Web.Scotty.Trans
  ) where

import Control.Applicative
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Servant.Prelude
import Servant.Response.Prelude
import Servant.Scotty.Arguments
import Servant.Scotty.Op
import Servant.Scotty.Response
import Web.Scotty.Trans

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
    (FromJSON a, Response (UpdateResponse Add) (r Add))

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
    (Index i, Response (UpdateResponse Delete) (r Delete))

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
    (Index i, FromJSON a, Response (UpdateResponse Update) (r Update))

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
