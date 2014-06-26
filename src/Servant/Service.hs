{-# LANGUAGE TypeOperators, DataKinds #-}

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
  , Service
  , runService
  , resource
  , module Servant.Context
  , module Servant.Error
  , module Servant.Operation
  , module Servant.Resource
  , module Servant.Response
  ) where

import Servant.Context
import Servant.Error
import Servant.Operation
import Servant.Resource
import Servant.Response
import Web.Scotty.Trans

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

newtype Service e m =
  Service { service :: ScottyT e m () }

resource :: Monad m
         => Resource m e c a i r (o ': ops)
         -> Service e m
         -> Service e m
resource r s = s { service = service s >> runResource r }

-- TODO: add a Bool argument for logging
runService :: Monad m
           => Service e m
           -> (e -> ActionT e m ())
           -> ScottyT e m ()
runService s h = do
  defaultHandler h
  service s
