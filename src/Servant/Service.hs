{-# LANGUAGE TypeOperators, DataKinds #-}

{- |
Module      :  Servant.Service
Copyright   :  (c) Zalora SEA 2014
License     :  BSD3
Maintainer  :  Alp Mestanogullari <alp@zalora.com>
Stability   :  experimental

Function for running a 'Service' composed of one or more 'Resource's in 'ScottyT'.
-}

module Servant.Service
  ( -- * Setting up a 'Service'
    Service
  , emptyService
  , resource
  , -- * Running a 'Service'
    runService
  , -- * Defining 'Servant.Resource.Resource's
    module Servant.Resource
  , -- * Standard operations for 'Resource's and building your own
    module Servant.Operation
  , -- * Properly handling exceptions in 'Resource's
    module Servant.Error
  , -- * Connection-like things: 'Context'
    module Servant.Context
  , -- * Some standard (JSON) response types, and defining your own
    module Servant.Response
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

-- | A simple 'Service' abstraction which is
--   just a container for 'Resource's.
--
--   * @e@ is the error type of your scotty application
--   * @m@ is the monad on top of which scotty sits
newtype Service e m =
  Service { service :: ScottyT e m () }

-- | A service with no 'Resource' in it.
emptyService :: Monad m => Service e m
emptyService = Service (return ())

-- | /Register/ (so to speak) a 'Resource' in the given 'Service'.
--
-- > companyInfoService :: Service Text IO
-- > companyInfoService =
-- >   emptyService & resource employeesResource
-- >                & resource clientsInfoResource
--
-- ('&' is defined in @Servant.Resource@ but also is exported from this module)
--
-- The resource must support at least one operation or your call won't typecheck.
resource :: Monad m
         => Resource m e c a i r (o ': ops)
         -> Service e m
         -> Service e m
resource r s = s { service = service s >> runResource r }

-- TODO: add a Bool argument for logging
-- | Run a 'Service' in a scotty monad
runService :: Monad m
           => Service e m           -- ^ service to run
           -> (e -> ActionT e m ()) -- ^ action to call when an error is 'raise'd
           -> ScottyT e m ()
runService s h = do
  defaultHandler h
  service s
