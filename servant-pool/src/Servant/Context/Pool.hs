{- |
Module      :  Servant.Context.Pool
Copyright   :  (c) Zalora SEA 2014
License     :  BSD3
Maintainer  :  Alp Mestanogullari <alp@zalora.com>
Stability   :  experimental

Create /servant/ 'Context's with pooling support
using <http://hackage.haskell.org/package/resource-pool resource-pool>
-}
module Servant.Context.Pool
  ( pooledContext
  , contextOfPool
  , NominalDiffTime
  ) where

import Data.Pool (Pool, withResource, createPool)
import Data.Time.Clock (NominalDiffTime)
import Servant.Context

-- | Use this function to get a 'Context'
--   using a 'Pool' you already have around.
--   Note that the type in the 'Context' is not
--   @Pool c@ but just @c@. 
--
--   It'll however use 'withResource' under the hood
--   to make a new connection available. That means
--   taking an unused one from the pool or bringing
--   a new one to life.
contextOfPool :: Pool c -> Context c
contextOfPool pool = mkContext (withResource pool)

-- | This is a handy function that lets you create the 'Context'
--   and the 'Pool' altogether. It just calls 'createPool' and
--   applies 'contextOfPool'.
pooledContext :: IO c            -- ^ Action that creates a new "connection"
			        -> (c -> IO ())    -- ^ Action that destroys an existing "connection"
              -> Int             -- ^ Number of stripes (sub-pools). /Minimum: 1/
              -> NominalDiffTime -- ^ Amount of time during which an unused
                                 --   "connection" is kept open
              -> Int             -- ^ Maximum number of resources to keep open
                                 --   per stripe. /Minimum: 1/
              -> IO (Context c)
pooledContext new destroy nstripes keepalive maxPerStripe =
  contextOfPool
    `fmap` createPool new destroy nstripes keepalive maxPerStripe
