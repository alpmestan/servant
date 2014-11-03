{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.API.Get where

import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Control.Monad.Cont
import Data.Aeson
import Data.Proxy
import Data.String.Conversions
import Data.Typeable
import Data.Void
import Network.HTTP.Types
import Network.URI
import Network.Wai
import Servant.Client
import Servant.Docs
import Servant.API.ContTypes
import Servant.Server
import Servant.Utils.Client

-- | Endpoint for simple GET requests. The server doesn't receive any arguments
-- and serves the contained type as JSON.
data Get a
  deriving Typeable

data GetHole = GetHole


instance ToJSON result => HasServer (Get result) (IsRouteMismatchH Void) where
  type Server (Get result) = EitherT (Int, String) IO result
  route Proxy action request respond
    | null (pathInfo request) && requestMethod request == methodGet = do
        e <- liftIO $ runEitherT action
        ContT $ \_ -> respond . succeedWith $ case e of
          Right output ->
            responseLBS ok200 [("Content-Type", "application/json")] (encode output)
          Left (status, message) ->
            responseLBS (mkStatus status (cs message)) [] (cs message)
    | null (pathInfo request) && requestMethod request /= methodGet =
        ContT $ \k -> k $ PartialMismatch $ ContT $ \_ -> respond $ failWith WrongMethod
    | otherwise = ContT $ \k -> k IsMismatch

instance FromJSON result => HasClient (Get result) where
  type Client (Get result) = URIAuth -> EitherT String IO result
  clientWithRoute Proxy req host =
    performRequest methodGet req 200 host

instance ToSample a => HasDocs (Get a) where
  docsFor Proxy (endpoint, action) =
    single endpoint' action'

    where endpoint' = endpoint & method .~ DocGET
          action' = action & response.respBody .~ toSample p
          p = Proxy :: Proxy a
