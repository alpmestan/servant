{-# LANGUAGE RankNTypes #-}

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
withContext c act = withctx c act
