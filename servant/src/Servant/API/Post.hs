{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.API.Post where

import Control.Monad.Trans.Either
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
import Servant.Server
import Servant.API.ContTypes
import Servant.Utils.Client

-- | Endpoint for POST requests. The type variable represents the type of the
-- response body (not the request body, use 'Servant.API.RQBody.RQBody' for
-- that).
data Post a
  deriving Typeable

instance ToJSON a => HasServer (Post a) (IsRouteMismatchH Void) where
  type Server (Post a) = EitherT (Int, String) IO a

  route Proxy action request respond
    | null (pathInfo request) && requestMethod request == methodPost = do
        e <- liftIO $ runEitherT action
        ContT $ \_ -> respond . succeedWith $ case e of
          Right out ->
            responseLBS status201 [("Content-Type", "application/json")] (encode out)
          Left (status, message) ->
            responseLBS (mkStatus status (cs message)) [] (cs message)
    | null (pathInfo request) && requestMethod request /= methodPost = ContT $ \k ->
        k $ PartialMismatch $ ContT $ \_ -> (respond $ failWith WrongMethod)
    | otherwise = ContT $ \k -> k IsMismatch

instance FromJSON a => HasClient (Post a) where
  type Client (Post a) = URIAuth -> EitherT String IO a

  clientWithRoute Proxy req uri =
    performRequest methodPost req 201 uri

instance ToSample a => HasDocs (Post a) where
  docsFor Proxy (endpoint, action) =
    single endpoint' action'

    where endpoint' = endpoint & method .~ DocPOST

          action' = action & response.respBody .~ toSample p
                           & response.respStatus .~ 201

          p = Proxy :: Proxy a
