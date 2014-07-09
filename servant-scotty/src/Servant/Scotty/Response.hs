{-# LANGUAGE FunctionalDependencies,
             MultiParamTypeClasses #-}
{- |
Module      :  Servant.Scotty.Response
Copyright   :  (c) Zalora SEA 2014
License     :  BSD3
Maintainer  :  Alp Mestanogullari <alp@zalora.com>
Stability   :  experimental

This module exports the 'Response' class and a handy 'respond'
function that you can use when defining handlers for your own
operations.

-}
module Servant.Scotty.Response
  ( -- * The 'Response' class
    module Servant.Response
  , -- * 'respond'
    respond
  ) where

import Servant.Response
import Web.Scotty.Trans

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
