module Servant.Service
 ( runResource ) where

import Web.Scotty.Trans
import Servant.Resource

runResource :: Resource m e c a i r ops
            -> ScottyT e m ()
runResource r = setupAction r
