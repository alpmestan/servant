{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.API.ContTypes where

import Data.Proxy
import Data.Void
import Control.Monad.Cont
import Network.Wai


data IsRouteMismatchH k
        = IsMismatch         -- I completely failed
        | PartialMismatch (ContT ResponseReceived IO Void)  -- I failed, but come back to
                                                         -- me if everyone else did too
        | NothingISRMH k     -- Matched

{-
instance HasServer a (IsRouteMismatchH k) => HasServer a k where
    type Server a = Server a
    route Proxy a request respond = withContT mp rac
      where mp fn = \(NothingISRMH k) -> fn k
            rac :: ContT ResponseReceived IO (IsRouteMismatchH k)
            rac = route Proxy a request respond
-}
