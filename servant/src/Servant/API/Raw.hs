{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.API.Raw where

import Control.Monad.Cont
import Data.Proxy
import Network.Wai
import Servant.Server

-- | Endpoint for plugging in your own Wai 'Application's.
--
-- The given 'Application' will get the request as received by the server, potentially with
-- a modified (stripped) 'pathInfo' if the 'Application' is being routed with 'Servant.API.Sub.:>'.
data Raw

data RawHole = RawHole

instance HasServer Raw RawHole where
  type Server Raw = Application
  route Proxy rawApplication request respond = ContT $ \_ ->
    rawApplication request (respond . succeedWith)
