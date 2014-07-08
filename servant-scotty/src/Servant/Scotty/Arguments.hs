{- |
Module      :  Servant.Scotty.Arguments
Copyright   :  (c) Zalora SEA 2014
License     :  BSD3
Maintainer  :  Alp Mestanogullari <alp@zalora.com>
Stability   :  experimental

Functions and typeclasses for automatically fetching
the arguments of the \"database operations\"
from either the request path or from the request body,
by decoding it from JSON to your type.
-}
module Servant.Scotty.Arguments
  ( -- * Deducing an argument from the request path
    Index(..)
  , -- * Decuding an argument from the JSON body
    js
  , FromJSON
  ) where

import Control.Monad.IO.Class
import Data.Aeson (FromJSON)
import Servant.Resource
import Web.Scotty.Trans

-- | What it means for a scotty 'Resource'
--   to have an index type.
--
--   * 'idx' should lookup in the request path
--     whatever is necessary to get the @i@
--     of @Resource c a i r e ops@, for operations
--     that take it as an argument, e.g /Delete/,
--     /Update/ or /View/.
--
--   * 'route' should return a 'String' that'll be
--     passed to 'capture'. You may use one or more
--     \"path parameters\" (calls to 'param', instances
--     of 'Param') to compute your value of type @k@.
--     You probably want to use 'name' on the 'Resource'
--     to generate the beginning of the path.
class Index k where
  -- | Lookup the index in the request path
  idx :: (MonadIO m, ScottyError e)
      => ActionT e m k

  -- | String to 'capture' that represents the
  --   'RoutePattern'.
  route :: Resource c a k r e ops -> String

-- | Simply gets the request's body as JSON
--   (or raises an exception if the decoding fails)
js :: (MonadIO m, ScottyError e, FromJSON a)
   => ActionT e m a
js = jsonData
