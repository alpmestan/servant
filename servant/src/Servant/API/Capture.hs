{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.API.Capture where

import Control.Monad.Cont
import Data.Proxy
import Data.Text
import GHC.TypeLits
import Network.Wai
import Servant.API.Sub
import Servant.Client
import Servant.Docs
import Servant.Server
import Servant.API.ContTypes
import Servant.Utils.Text

-- * Captures
data Capture sym a

captured :: FromText a => proxy (Capture sym a) -> Text -> Maybe a
captured _ = fromText

instance (KnownSymbol capture, FromText a, HasServer sublayout (IsRouteMismatchH k))
      => HasServer (Capture capture a :> sublayout) (IsRouteMismatchH k) where

  type Server (Capture capture a :> sublayout) =
     a -> Server sublayout

  route Proxy subserver request respond = case pathInfo request of
    (first : rest)
      -> case captured captureProxy first of
           Nothing  -> ContT $ \k -> k IsMismatch
           Just v   -> route (Proxy :: Proxy sublayout) (subserver v) request{
                         pathInfo = rest
                       } respond
    _ -> ContT $ \k -> k IsMismatch

    where captureProxy = Proxy :: Proxy (Capture capture a)

instance (KnownSymbol capture, ToText a, HasClient sublayout)
      => HasClient (Capture capture a :> sublayout) where

  type Client (Capture capture a :> sublayout) =
    a -> Client sublayout

  clientWithRoute Proxy req val =
    clientWithRoute (Proxy :: Proxy sublayout) $
      appendToPath p req

    where p = unpack (toText val)

instance (KnownSymbol sym, ToCapture (Capture sym a), HasDocs sublayout)
      => HasDocs (Capture sym a :> sublayout) where

  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint', action')

    where sublayoutP = Proxy :: Proxy sublayout
          captureP = Proxy :: Proxy (Capture sym a)

          action' = over captures (|> toCapture captureP) action
          endpoint' = over path (\p -> p++"/:"++symbolVal symP) endpoint
          symP = Proxy :: Proxy sym
