{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Servant.API.Delete where

import Control.Monad.Trans.Either
import Data.Proxy
import Data.String.Conversions
import Data.Typeable
import Network.HTTP.Types
import Network.Wai
import Servant.Server

-- | Combinator for DELETE requests.
--
-- Example:
--
-- >            -- DELETE /books/:isbn
-- > type MyApi = "books" :> Capture "isbn" Text :> Delete
data Delete
  deriving Typeable

-- | If you have a 'Delete' endpoint in your API,
-- the handler for this endpoint is meant to delete
-- a resource.
--
-- The code of the handler will, just like
-- for 'Servant.API.Get.Get', 'Servant.API.Post.Post' and
-- 'Servant.API.Put.Put', run in @EitherT (Int, String) IO ()@.
-- The 'Int' represents the status code and the 'String' a message
-- to be returned. You can use 'Control.Monad.Trans.Either.left' to
-- painlessly error out if the conditions for a successful deletion
-- are not met.
instance HasServer Delete where
  type Server Delete = EitherT (Int, String) IO ()

  route Proxy action request respond
    | null (pathInfo request) && requestMethod request == methodDelete = do
        e <- runEitherT action
        respond $ succeedWith $ case e of
          Right () ->
            responseLBS status204 [] ""
          Left (status, message) ->
            responseLBS (mkStatus status (cs message)) [] (cs message)
    | null (pathInfo request) && requestMethod request /= methodDelete =
        respond $ failWith WrongMethod
    | otherwise = respond $ failWith NotFound
