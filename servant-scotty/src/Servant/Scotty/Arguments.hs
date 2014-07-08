module Servant.Scotty.Arguments where

import Control.Monad.IO.Class
import Data.Aeson (FromJSON)
import Servant.Resource
import Web.Scotty.Trans

class Index k where
  idx :: (MonadIO m, ScottyError e)
      => ActionT e m k

  route :: Resource c a k r e ops -> String

js :: (MonadIO m, ScottyError e, FromJSON a)
   => ActionT e m a
js = jsonData
