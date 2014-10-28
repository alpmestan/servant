{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Servant.API.Mount where

import Data.Proxy
import Data.String.Conversions
import GHC.TypeLits
import Network.Wai
import Servant.Client
import Servant.Docs
import Servant.Server
import Servant.API.Sub

data Mount path

instance (KnownSymbol path, HasServer sublayout) => HasServer (Mount path :> sublayout) where
  type Server (Mount path :> sublayout) = Server sublayout
  route Proxy subserver request respond = case pathInfo request of
    (first : rest)
      | first == cs (symbolVal proxyPath)
      -> route (Proxy :: Proxy sublayout) subserver request{
           pathInfo = rest
         } respond
    _ -> respond Nothing

    where proxyPath = Proxy :: Proxy path

instance (KnownSymbol path, HasClient sublayout) => HasClient (Mount path :> sublayout) where
  type Client (Mount path :> sublayout) = Client sublayout

  clientWithRoute Proxy req =
     clientWithRoute (Proxy :: Proxy sublayout) $
       appendToPath p req

    where p = symbolVal (Proxy :: Proxy path)

instance (KnownSymbol path, HasDocs sublayout) => HasDocs (Mount path :> sublayout) where

  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint', action)

    where sublayoutP = Proxy :: Proxy sublayout
          endpoint' = endpoint & path <>~ symbolVal pa
          pa = Proxy :: Proxy path
