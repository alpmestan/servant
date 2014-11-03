{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.API.Alternative where

import Control.Monad.Cont
import Data.Proxy
import Data.Void
import Network.Wai
import Servant.Client
import Servant.Docs
import Servant.Server
import Servant.API.ContTypes

import Debug.Trace
-- | Union of two APIs, first takes precedence in case of overlap.
data a :<|> b = a :<|> b
infixr 8 :<|>


instance ( HasServer a (IsRouteMismatchH k)
         , HasServer b (IsRouteMismatchH k))
         => HasServer (a :<|> b) (IsRouteMismatchH k) where
  type Server (a :<|> b) = Server a :<|> Server b
  route Proxy (a :<|> b) request respond = do
      optA <- route pa a request respond
      optB <- route pb b request respond
      case optA of
            IsMismatch -> trace "mismatch a" $ callCC $ \cc -> cc optB
            PartialMismatch k -> trace "partialmismatch a" $ do
                case optB of
                    IsMismatch -> trace "mismatch b" $ callCC $ \cc -> cc optA
                    PartialMismatch k' -> trace "partialmismatch b" $ callCC $ \cc -> cc optB
            -- NothingISRMH k -> ContT $ \fk -> fk k

    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b

instance (HasClient a, HasClient b) => HasClient (a :<|> b) where
  type Client (a :<|> b) = Client a :<|> Client b
  clientWithRoute Proxy req =
    clientWithRoute (Proxy :: Proxy a) req :<|>
    clientWithRoute (Proxy :: Proxy b) req

instance (HasDocs layout1, HasDocs layout2)
      => HasDocs (layout1 :<|> layout2) where

  docsFor Proxy (ep, action) = docsFor p1 (ep, action) <> docsFor p2 (ep, action)

    where p1 :: Proxy layout1
          p1 = Proxy

          p2 :: Proxy layout2
          p2 = Proxy
