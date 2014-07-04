{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Scotty
  ( runResource
  , Runnable
  , ScottyOp(..)
  ) where

import Control.Monad.IO.Class
import Servant.Resource
import Web.Scotty.Trans

-- | Internal class used to drive the recursion
--   on the list of operations.
--
--   This lets everyone care only about the specific
--   behavior of their operations.
--
--   Regardless of what's
--   specific about each operation, we just recursively
--   go through all the operations and call 'runOperation'
--   for each of them.
class Runnable ops where
  -- | Call this function to setup a 'Resource' in your
  --   scotty application.
  runResource :: (MonadIO m, ScottyError e)
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

-- | A class that lets you define a handler for an operation @o@.
class ScottyOp o where
  -- | Given a 'Resource' and the \"database function\" (so to speak)
  --   corresponding to your operation, do some business in /scotty/'s
  --   'ScottyT' and 'ActionT' monads to define a handler for this very operation.
  --
  --   To provide the \"database function\" with some 'Context' @c@
  --   you can use 'Servant.Context.withContext' to run the operation
  --   and 'Servant.Resource.context' to get the context of your 'Resource'.
  --
  --   To catch exceptions around your db operation in your handler,
  --   you can use the 'Servant.Resource.excCatcher' access the
  --   'Servant.Error.ExceptionCatcher' of your 'Resource' and
  --   'Servant.Error.handledWith' to catch them and convert them
  --   to your error type @e@. You can then 'raise' the error value
  --   if you have a sensible default handler or handle it locally and
  --   respond with whatever is appropriate in your case.
  runOperation :: (MonadIO m, ScottyError e)
               => Resource c a i r e (o ': ops)
               -> Operation o c a i r
               -> ScottyT e m ()
