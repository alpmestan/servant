{-# LANGUAGE RankNTypes #-}
{- |
Module      :  Servant.Context
Copyright   :  (c) Zalora SEA 2014
License     :  BSD3
Maintainer  :  Alp Mestanogullari <alp@zalora.com>
Stability   :  experimental

A 'Context' type for holding a function
that will use some /context/ to execute
something analoguous to a /database operation/.

This is equivalent to holding something like:

> withConnection someConnection

where you have previously set up @someConnection@ by specifying a
connection string or something like that. This lets us support
any kind of backend (even an in-memory Map in an /IORef/), and
most interestingly, we can use just raw connections or a connection
pool, this approach really covers a lot of situations while keeping
everything quite simple.

-}
module Servant.Context
  ( Context
  , mkContext
  , withContext
  ) where

-- | A 'Context' is just a wrapper around a function
--   that can execute IO operations /given/
--   this context.
newtype Context c
  = Context { withctx :: forall r. (c -> IO r) -> IO r }

-- | Create a 'Context' from a suitable function
mkContext :: (forall r. (c -> IO r) -> IO r)
          -> Context c
mkContext = Context

-- | Use the 'Context' to actually perform an 'IO'
--   operation requiring it.
withContext :: Context c
            -> (c -> IO r)
            -> IO r
withContext = withctx
