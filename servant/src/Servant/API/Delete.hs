{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.API.Delete where

import Control.Monad.Cont
import Control.Monad.Trans.Either
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

-- | Endpoint for DELETE requests.
data Delete
  deriving Typeable

instance HasServer Delete (IsRouteMismatchH Void) where
  type Server Delete = EitherT (Int, String) IO ()

  route Proxy action request respond
    | null (pathInfo request) && requestMethod request == methodDelete = ContT $ \k -> do
        e <- runEitherT action
        respond $ succeedWith $ case e of
          Right () ->
            responseLBS status204 [] ""
          Left (status, message) ->
            responseLBS (mkStatus status (cs message)) [] (cs message)
    | null (pathInfo request) && requestMethod request /= methodDelete = ContT $ \_ ->
        respond $ failWith WrongMethod
    | otherwise = ContT $ \_ -> respond $ failWith NotFound

instance HasClient Delete where
  type Client Delete = URIAuth -> EitherT String IO ()

  clientWithRoute Proxy req host =
    performRequest methodDelete req 204 host

instance HasDocs Delete where
  docsFor Proxy (endpoint, action) =
    single endpoint' action'

    where endpoint' = endpoint & method .~ DocDELETE

          action' = action & response.respBody .~ Nothing
                           & response.respStatus .~ 204
