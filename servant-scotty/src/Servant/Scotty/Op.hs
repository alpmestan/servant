{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Servant.Scotty.Op where

import Control.Monad.IO.Class
import GHC.Exts
import Servant.Context
import Servant.Error
import Servant.Resource
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

safely :: (MonadIO m, ScottyError e)
       => Resource c a i r e ops
       -> ActionT e m (c -> IO x)
       -> ActionT e m x
safely res mact = do
  act <- mact
  result <- liftIO $ withContext (context res) act
              `handledWith` excCatcher res
  either raise return result
