{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      :  Servant.Scotty
Copyright   :  (c) Zalora SEA 2014
License     :  BSD3
Maintainer  :  Alp Mestanogullari <alp@zalora.com>
Stability   :  experimental

Module for defining a <http://hackage.haskell.org/package/scotty scotty>
webservice from 'Resource's.

> EXAMPLE HERE
-}
module Servant.Scotty
  ( -- * Setting up handlers for a 'Resource'
    Runnable(runResource)
    -- * Defining handlers for an operation
  , ScottyOp(..)
  , Index(..)
  , js
  , safely
  , Response(..)
  , respond
    -- * Utilities
  , Suitables
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import GHC.Exts
import Servant.Resource
import Servant.Scotty.Arguments
import Servant.Scotty.Op
import Servant.Scotty.Response
import Web.Scotty.Trans

-- | Internal class used to drive the recursion
--   on the list of operations when generating all
--   the endpoints and handlers.
--
--   This lets everyone care only about the specific
--   behavior of their operations, this class will make
--   sure all the operations of your 'Resource' have an
--   implementation.
--
--   Regardless of what's
--   specific about each operation, we just recursively
--   go through all the operations and call 'runOperation'
--   for each of them.
class Runnable ops where
  -- | Call this function to setup a 'Resource' in your
  --   scotty application.
  runResource :: (Functor m, MonadIO m, ScottyError e, Suitables ops a i r)
              => Resource c a i r e ops
              -> ScottyT e m ()

-- | No operation means we don't setup any handler.
instance Runnable '[] where
  -- runResource :: (MonadIO m, ScottyError e)
  --             => Resource c a i r e '[]
  --             -> ScottyT e m ()
  -- no operation supported
  -- (or "no more", if there was any and we're ending the recursion)
  runResource _ = return ()

-- | Given some already runnable operation list @ops@,
--   and an operation that we can run in scotty
--
--   (that's the @'ScottyOp' o@ constraint),
--
--   we can run the @(o ': ops)@ operation list.
instance (ScottyOp o, Runnable ops) => Runnable (o ': ops) where
  -- runResource :: (MonadIO m, ScottyError e)
	--   					 => Resource c a i r e (o ': ops)
	--	    			 -> ScottyT e m ()
  runResource r = do
    withHeadOperation r runOperation
    runResource (dropHeadOperation r)

type family Suitables (ops :: [*]) a i r :: Constraint
type instance Suitables '[] a i r = ()
type instance Suitables (o ': ops) a i r = (Suitable o a i r, Suitables ops a i r)
