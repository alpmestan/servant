{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Servant.API.Sub where

import Control.Monad.Cont
import Data.Proxy
import Data.String.Conversions
import GHC.TypeLits
import Network.Wai
import Servant.Client
import Servant.Docs
import Servant.Server
import Servant.API.ContTypes

-- | The contained API (second argument) can be found under @("/" ++ path)@
-- (path being the first argument).
data (path :: k) :> a = Proxy path :> a
infixr 9 :>

data SubHole = SubHole

instance (KnownSymbol path, HasServer sublayout (IsRouteMismatchH s))
         => HasServer (path :> sublayout) (IsRouteMismatchH s) where
  type Server (path :> sublayout) = Server sublayout
  route Proxy subserver request respond = case pathInfo request of
    (first : rest)
      | first == cs (symbolVal proxyPath)
      -> route (Proxy :: Proxy sublayout) subserver request{
           pathInfo = rest
         } respond
    _ -> ContT $ \k -> k IsMismatch

    where proxyPath = Proxy :: Proxy path

instance (KnownSymbol path, HasClient sublayout) => HasClient (path :> sublayout) where
  type Client (path :> sublayout) = Client sublayout

  clientWithRoute Proxy req =
     clientWithRoute (Proxy :: Proxy sublayout) $
       appendToPath p req

    where p = symbolVal (Proxy :: Proxy path)

instance (KnownSymbol path, HasDocs sublayout) => HasDocs (path :> sublayout) where

  docsFor Proxy (endpoint, action) =
    docsFor sublayoutP (endpoint', action)

    where sublayoutP = Proxy :: Proxy sublayout
          endpoint' = endpoint & path <>~ symbolVal pa
          pa = Proxy :: Proxy path
