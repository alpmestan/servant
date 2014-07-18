{-# LANGUAGE FunctionalDependencies,
             MultiParamTypeClasses #-}
{- |
Module      :  Servant.Response
Copyright   :  (c) Zalora SEA 2014
License     :  BSD3
Maintainer  :  Alp Mestanogullari <alp@zalora.com>
Stability   :  experimental

This module contains a generic 'Response' class for tying the result
of some \"database operation\" to some response type of yours,
or to the standard ones from "Servant.Scotty.Prelude" for example.

-}
module Servant.Response
  ( -- * The 'Response' class
    Response(..)
  , -- * Useful for defining your instances
    module Network.HTTP.Types.Status
  ) where

import Data.Aeson hiding (json)
import Network.HTTP.Types.Status

-- | A class that ties return types of your database operations
--   and the output that will be generated to communicate
--   the result. 
--
-- * The first type, @resp@, is the response type that will be encoded
--   in JSON and sent as the response body.
--
-- * The second type, @result@, is the result type of your \"database\"
--   or \"context\" operation.
--
--   For example, if you're adding an item, and if you're using
--   postgresql-simple, you'll probably want to use the
--   'Response' instances defined in the servant-postgresql package,
--   in the @Servant.PostgreSQL.Prelude@ module.
--
--   It lets you specify, given a value of your result, if no
--   exception is thrown, what response should be sent as JSON
--   to the client along with what HTTP status.
--
--   There's a functional dependency at play: the result type
--   of a database operation determines the representation that'll be
--   picked for generating the json output.
class ToJSON resp => Response resp result | result -> resp where
  toResponse :: result -> (resp, Status)
