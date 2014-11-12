{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.API.File where

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Proxy
import Data.Text (pack)
import Data.Typeable
import GHC.TypeLits
import Network.HTTP.Types
import Network.Mime
import Network.URI
import Network.Wai
import Servant.API.Sub
import Servant.Client
import Servant.Common.BaseUrl
import Servant.Common.Req
import Servant.Docs
import Servant.Server
import System.Directory
import System.Environment
import System.FilePath

import qualified Data.ByteString.Lazy.Char8 as LB

-- | > servantFilesEnv = "SERVANT_FILES"
servantFilesEnv :: String
servantFilesEnv = "SERVANT_FILES"

type BaseDir = String

-- | Serve @$SERVANT_FILES/filename@, where @$SERVANT_FILES@ is an
--   environment variable you have to set.
data File filename
  deriving Typeable

-- | A "dummy" handler for serving static files.
--
-- > type MySite = Static "cv.html"  -- under /cv.html, serve cv.html
-- >          :<|> File "index.html" -- under /, serve "index.html"
-- >
-- > server :: Server MySite
-- > server = file :<|> file
file :: ()
file = ()

-- | Equivalent to @filename :> 'File' filename@.
--
-- > Static "cv.html" :<|> Static "contact.html"
--
-- is equivalent to:
--
-- >        "cv.html"      :> File "cv.html"        -- /cv.html
-- >   :<|> "contact.html" :> File "contact.html"   -- /contact.html
--
-- And would serve @cv.html@ under @/cv.html@ and @contact.html@ under @/contact.html@.
type Static filename = filename :> File filename

instance KnownSymbol filename => HasServer (File filename) where
  type Server (File filename) = ()

  route Proxy () request respond
    | null (pathInfo request) && requestMethod request == methodGet = do
        servantFiles <- lookupEnv servantFilesEnv
        case servantFiles of
          Nothing  -> respond $ failWith NotFound
          Just dir -> do
            let file = dir </> filename
            fileExists <- liftIO $ doesFileExist file
            if fileExists
              then respond . succeedWith $
                     responseFile ok200 mimeHeader file Nothing
              else respond $ failWith NotFound

    | null (pathInfo request) && requestMethod request /= methodGet =
        respond $ failWith WrongMethod
    | otherwise = respond $ failWith NotFound


    where filename = symbolVal (Proxy :: Proxy filename)
          mimeHeader = [("Content-Type", mime)]
          mime = defaultMimeLookup (pack filename)

instance KnownSymbol filename => HasClient (File filename) where
  type Client (File filename) = BaseUrl -> EitherT String IO LB.ByteString
  clientWithRoute Proxy req host =
    -- fixme: we don't add "Accept" headers yet
    performRequest' methodGet req 200 host

instance KnownSymbol filename => HasDocs (File filename) where
  docsFor Proxy (endpoint, action) =
    single endpoint' action'

    where endpoint' = endpoint & method .~ DocGET
          action' = action & response.respBody .~ Just ("<Content of $SERVANT_FILES/" <> filename <> ">")
          filename = LB.pack $ symbolVal (Proxy :: Proxy filename)
