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

This module contains the class that you must implement
if you want some operation supported by the scotty backend and
some utilities you'll most likely need if you want to define your
own instances.

Import @Servant.Scotty.Prelude@ if you want instances for the
REST-y operations defined in @Servant.Prelude@.
-}
module Servant.Scotty.Op
	( -- * The 'ScottyOp' class
    ScottyOp(..)
  , -- * Utility functions/classes you'll want to use
    --   for you own instances
    safely
  , Response(..)
  , respond
  , Index(..)
  , js
  ) where

import Control.Monad.IO.Class
import GHC.Exts
import Servant.Context
import Servant.Error
import Servant.Resource
import Servant.Scotty.Arguments
import Servant.Scotty.Response
import Web.Scotty.Trans

-- | A class that lets you define a handler for an operation @o@.
class ScottyOp o where
  -- | Each operation can define its own constraints on:
  --
  --   * the type of the entries, @a@
  --   * the type by which the entries are indexed, @i@
  --   * the result type @r@ of \"effectful\" database operations
  --     (those that add/update/delete entries)
  --
  --   This is useful because that way, your types will /only/ have to
  --   satisfy the constraints /specified/ by the operations your 'Resource'
  --   carries, not some global dumb constraints you have to pay for even if
  --   you don't care about the operation that requires this constraint.
  type Suitable o a i r :: Constraint

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
  runOperation :: (Functor m, MonadIO m, ScottyError e, Suitable o a i r)
               => Resource c a i r e (o ': ops)
               -> Operation o c a i r
               -> ScottyT e m ()

-- | This is a function you'll want to use when defining your
--   own operations.
--
--   It runs the second argument to get the operation to run,
--   and feeds it the 'Resource' argument's context and extracts
--   the result.
--
--   Intended to be used in 'runOperation' in a way similar to
--
--   > -- example: for the 'Delete' operation
--   > runOperation res op =
--   >   delete (capture $ "/" ++ name res ++ route res) $ do
--   >     result <- safely res $ op <$> idx
--   >     respond result
--
--   Here we just take the particular delete operation for the
--   client code's type, lookup the 'Index' argument it takes
--   from the request path and run the operation 'safely', eventually
--   converting its result to an appropriate response.
safely :: (MonadIO m, ScottyError e)
       => Resource c a i r e ops  -- ^ the resource, that holds the context @c@
                                  --   and the exception catchers required to run
                                  --   the operation
       -> ActionT e m (c -> IO x) -- ^ a scotty action that'll produce the operation
                                  --   we want, with all the arguments but the context
                                  --   already provided.
       -> ActionT e m x           -- ^ returns the result of the operation
                                  --   or 'raise' if an exception the 'Resource'
                                  --   is watching for is thrown, using the appropriate conversion
                                  --   to your application's error type @e@.
safely res mact = do
  act <- mact
  result <- liftIO $ withContext (context res) act
              `handledWith` excCatcher res
  either raise return result
