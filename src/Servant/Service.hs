{- |
Module      :  Servant.Service
Copyright   :  (c) Zalora SEA 2014
License     :  BSD3
Maintainer  :  Alp Mestanogullari <alp@zalora.com>
Stability   :  experimental

Function for running a 'Resource' in 'ScottyT'.
-}

module Servant.Service
  ( runResource
  , module Servant.Context
  , module Servant.Operation
  , module Servant.Resource
  , module Servant.Response
  ) where

import Web.Scotty.Trans
import Servant.Context
import Servant.Operation
import Servant.Resource
import Servant.Response

-- | Set up all the handlers for this resource
--
--   This is morally equivalent to doing:
--
-- > get "/items" $ do { ... } -- list items
-- > post "/items" $ do { ... } -- add an item
-- > -- etc
runResource :: Resource m e c a i r ops
            -> ScottyT e m ()
runResource r = setupAction r
