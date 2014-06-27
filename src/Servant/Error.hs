{-# LANGUAGE ExistentialQuantification #-}
{- |
Module      :  Servant.Error
Copyright   :  (c) Zalora SEA 2014
License     :  BSD3
Maintainer  :  Alp Mestanogullari <alp@zalora.com>
Stability   :  experimental

Everything you'll need when doing some kind of exception handling
around your "database operations".
-}
module Servant.Error
  ( -- * Creating an combining 'ExceptionCatcher's
    ExceptionCatcher
  , noErrorHandling
  , catchAnd
  , combineCatchers

  , -- * Running 'IO' actions against an 'ExceptionCatcher'
    handledWith
  , -- * Catching exceptions and raising them as an error in scotty
    raiseIfExc
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Monoid
import Web.Scotty.Trans

-- | A container for zero or more "exception catchers".
--
--   By exception catcher, we mean here a function that can convert
--   some precise instance of 'Exception' to the error type used in
--   your scotty app (the @e@ type in "Servant.Resource" "Servant.Service")
newtype ExceptionCatcher err =
	EC { catchers :: [Catcher err] }

-- a handy type for representing a single function that can catch
-- some (specific) exception.
-- The dictionary for the corresponding 'Exception' instance is packed
-- along with the function in this data type.
data Catcher err = forall exc. Exception exc => Catcher (exc -> err)

-- | Don't catch any exception.
noErrorHandling :: ExceptionCatcher err
noErrorHandling = EC []

-- | Combine two 'ExceptionCatcher'.
--
--   The resulting 'ExceptionCatcher' will be able to catch
--   exceptions using the /catchers/ from both arguments.
combineCatchers :: ExceptionCatcher err
                -> ExceptionCatcher err
                -> ExceptionCatcher err
combineCatchers (EC c1s) (EC c2s) = EC (c1s ++ c2s)

-- | If you're into 'Monoid's, enjoy our
--   'mempty' ('noErrorHandling') and '<>' ('combineCatchers').
instance Monoid (ExceptionCatcher err) where
  mempty = noErrorHandling

  mappend = combineCatchers

-- | Run an 'IO' action by watching for all the exceptions
--   supported by the 'ExceptionCatcher'.
--
--   If an exception is thrown somewhere in the action and is caught
--   by the catcher, it will call the function given to 'catchAnd'
--   to convert the exception value to the error type of your scotty application.
--
--   If an exception is thrown whose type isn't one for which there's a /catcher/
--   in the 'ExceptionCatcher' argument, you'll have to catch it yourself or let it
--   be sent to the default handler.
--
--   Since 'Servant.Service.runService' sets up a 'defaultHandler', you'll most
--   likely want to directly use 'raiseIfExc' which 'raise's the error value
--   you got from the exception if some exception is thrown or just run some
--   scotty action if everything went smoothly. This means writing a handler
--   that'll set up a response with the proper HTTP status and send some JSON
--   to the client with an informative error message, which... isn't a bad thing :-)
handledWith :: IO a -> ExceptionCatcher err -> IO (Either err a)
handledWith act (EC hs) = fmap Right act `catches` map runCatcher hs

  where runCatcher :: Catcher err -> Handler (Either err a)
        runCatcher (Catcher f) = Handler $ return . Left . f

-- | Create an 'ExceptionCatcher' from a function.
--
--   This will make the catcher aware of exceptions of type @except@
--   so that when you'll run a catcher against an 'IO' action
--   using 'raiseIfExc' or 'handledWith', if an exception of this type
--   is thrown, it will be caught and converted to the error type of your
--   web application using the provided function.
catchAnd :: (Exception except, ScottyError err)
         => (except -> err) 
         -> ExceptionCatcher err
catchAnd f = EC $ [catcher]
  where catcher = Catcher f

-- | Run an 'IO' action against zero or more catchers,
--   so that if an exception arises and is handled by one of the
--   catchers, it gets 'raise'd in your scotty application using
--   the conversion function your provided in the call to 'catchAnd'.
--
--   If no exception is thrown, it executes the supplied scotty 'Action'.
--
--   If an exception type for which you don't provide a catcher is thrown,
--   it'll propagate and most likely hit scotty's 'defaultHandler' as a dumb
--   string, so you'll still know about it but you won't be able to do a proper
--   translation.
raiseIfExc :: (ScottyError e, MonadIO m)
           => ExceptionCatcher e
           -> IO a
           -> (a -> ActionT e m ())
           -> ActionT e m ()
raiseIfExc cs act ifok = do
  res <- liftIO $ act `handledWith` cs
  either raise ifok res
