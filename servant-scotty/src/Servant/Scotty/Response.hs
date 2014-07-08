{-# LANGUAGE FunctionalDependencies,
             MultiParamTypeClasses #-}
{- |
Module      :  Servant.Scotty.Response
Copyright   :  (c) Zalora SEA 2014
License     :  BSD3
Maintainer  :  Alp Mestanogullari <alp@zalora.com>
Stability   :  experimental

This module contains a generic 'Response' class for tying the result
of some \"database operation\" to some response type of yours,
or to the standard ones from "Servant.Scotty.Prelude".

-}
module Servant.Scotty.Response
  ( -- * The 'Response' class
    Response(..)
  , -- * 'respond'
    respond
  , -- * For defining your instances
    module Network.HTTP.Types.Status
  ) where

import Data.Aeson hiding (json)
import Network.HTTP.Types.Status
import Web.Scotty.Trans

-- | A class that ties return types of your database operations
--   and the output format that will be used to communicate
--   the result. 
--
-- * The first type, @resp@, is the response type that will be encoded
--   in JSON and sent as the response body.
--
-- * The second type, @result@, is the result type of your \"database\"
--   or \"context\" operation.
--
--   For example, if you're adding an item, and if you're using
--   postgresql-simple, you'll probably want to write an instance like:
--
-- > instance Response (UpdateResponse Add) [Only Int] where
--
--   because @[Only Int]@ is what postgresql-simple's @execute@ returns.
--
--   It lets you specify, given a value of your result, if no
--   exception is thrown, what response should be sent as JSON
--   to the client along with what HTTP status.
class ToJSON resp => Response resp result | result -> resp where
  toResponse :: result -> (resp, Status)

-- | Given the result of some operation,
--   it picks the appropriate response type
--   and uses 'toResponse' to convert the result
--   to a JSON-encodable value along with a status code,
--   both used to then send a response to the client.
respond :: (Response resp x, ScottyError e, Monad m)
        => x
        -> ActionT e m ()
respond result = do
  let (respValue, statuscode) = toResponse result
  status statuscode
  json respValue
