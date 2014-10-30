{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Servant.API.ReqBody where

import Control.Applicative
import Control.Monad.Cont
import Data.Aeson
import Data.Proxy
import Network.Wai
import Servant.API.Sub
import Servant.Client
import Servant.Docs
import Servant.Server

-- * Request Body support
data ReqBody a


data Hole a layout = Resp (IO ResponseReceived) | Fn (a -> Server layout)

instance (FromJSON a, HasServer sublayout)
      => HasServer (ReqBody a :> sublayout) where

  type Server (ReqBody a :> sublayout) = ContT ResponseReceived IO (Hole a sublayout)

  route Proxy subserver request respond = do
    runContT subserver
            (\x -> case x of
              Resp x -> x
              Fn fn -> do
                     mrbod <- decode' <$> lazyRequestBody request
                     case mrbod of
                       Nothing -> respond $ failWith InvalidBody
                       Just a -> route (Proxy :: Proxy sublayout) (fn a) request respond
              )


instance (ToJSON a, HasClient sublayout)
      => HasClient (ReqBody a :> sublayout) where

  type Client (ReqBody a :> sublayout) =
    a -> Client sublayout

  clientWithRoute Proxy req body =
    clientWithRoute (Proxy :: Proxy sublayout) $
      setRQBody (encode body) req

instance (ToSample a, HasDocs sublayout)
      => HasDocs (ReqBody a :> sublayout) where

  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint, action')

    where sublayoutP = Proxy :: Proxy sublayout

          action' = action & rqbody .~ toSample p
          p = Proxy :: Proxy a
