{-# LANGUAGE ExistentialQuantification #-}
module Servant.Error where

import Control.Exception
import Control.Monad.IO.Class
import Data.Monoid
import Web.Scotty.Trans

newtype ExceptionCatcher err =
	EC { catchers :: [Catcher err] }

data Catcher err = forall exc. Exception exc => Catcher (exc -> err)

noErrorHandling :: ExceptionCatcher err
noErrorHandling = EC []

combineHandlers :: ExceptionCatcher err
                -> ExceptionCatcher err
                -> ExceptionCatcher err
combineHandlers (EC c1s) (EC c2s) = EC (c1s ++ c2s)

instance Monoid (ExceptionCatcher err) where
  mempty = noErrorHandling

  mappend = combineHandlers

handledWith :: IO a -> ExceptionCatcher err -> IO (Either err a)
handledWith act (EC hs) = fmap Right act `catches` map runCatcher hs

  where runCatcher :: Catcher err -> Handler (Either err a)
        runCatcher (Catcher f) = Handler $ return . Left . f

catchAnd :: (Exception except, ScottyError err)
         => (except -> err) 
         -> ExceptionCatcher err
catchAnd f = EC $ [catcher]
  where catcher = Catcher f

raiseIfExc :: (ScottyError e, MonadIO m)
           => ExceptionCatcher e
           -> IO a
           -> (a -> ActionT e m ())
           -> ActionT e m ()
raiseIfExc cs act ifok = do
  res <- liftIO $ act `handledWith` cs
  either raise ifok res
