{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Servant.API.End where

import Data.Proxy
import Data.Void
import Control.Monad.Cont
import Network.HTTP.Types
import Network.URI
import Network.Wai

import Servant.Server

data End = End

instance HasServer End Void where
   type Server End = ()
   route Proxy _ request respond = ContT $ \_ -> respond $ failWith NotFound
